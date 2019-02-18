;;; chip8.el --- A CHIP-8 Emulator  -*- firestarter: (byte-compile-file (buffer-file-name)); -*-

;; Copyright (C) 2019 Vasilij Schneidermann <mail@vasilij.de>

;; Author: Vasilij Schneidermann <mail@vasilij.de>
;; URL: https://github.com/wasamasa/chip-8
;; Version: 0.0.1
;; Package-Requires:
;; Keywords: games

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is an emulator for the (Super) CHIP-8 VM which is capable of
;; playing plenty of classic games on modest machines, such as the
;; infinitely customizable text editor.  Use M-x chip8-emulate and
;; select a ROM file to be played.  You may want to customize
;; `chip8-keys' (to select a keymap fitting your keyboard),
;; `chip8-key-timeout' (in case key repeat doesn't feel right) and
;; `chip8-speed-factor' (to increase the speed for slow ROMs).

;;; Code:

(defgroup chip8 nil
  "A CHIP-8 Emulator"
  :group 'games
  :prefix "chip8-")

(defvar chip8-debug nil)
(defconst chip8-debug-buffer "*CHIP-8 debug*")

(defconst chip8-buffer "*CHIP-8*")
(defconst chip8-program-start #x200)

(defcustom chip8-ignore-second-shift-arg t
  "If non-nil, emulate a wide-spread bug in shift ops.
t: Vx = Vx SHR/SHL 1
nil: Vx = Vy SHR/SHL 1"
  :type 'boolean
  :group 'chip8)

(defcustom chip8-speed-factor 10
  "Amount of cycles to execute on each timer run.
As the timer runs at 60hz, factor 1 corresponds to 60 cps, factor
2 to 120 cps, etc."
  :type 'integer
  :group 'chip8)

(defvar chip8-regs
  (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; V0-VF
          0 ; I
          chip8-program-start ; PC
          0 ; SP
          0 ; DT
          0 ; ST
          ))
(defvar chip8-ram (make-vector #xFFF 0))
(defvar chip8-stack (make-vector 32 0))
(defvar chip8-RPL-flags (make-vector 16 0))

(defconst chip8-fb-width 64)
(defconst chip8-fb-height 32)
(defconst chip8-fb-size (* chip8-fb-height chip8-fb-width))

(defconst chip8-xfb-width 128)
(defconst chip8-xfb-height 64)
(defconst chip8-xfb-size (* chip8-xfb-height chip8-xfb-width))

(defvar chip8-fb (make-vector chip8-fb-size 0))
(defvar chip8-old-fb (make-vector chip8-fb-size -1))
(defvar chip8-xfb (make-vector chip8-xfb-size 0))
(defvar chip8-old-xfb (make-vector chip8-xfb-size -1))
(defvar chip8-fb-dirty-p nil)

(defconst chip8-sprites
  (vector #xF0 #x90 #x90 #x90 #xF0 ; 0
          #x20 #x60 #x20 #x20 #x70 ; 1
          #xF0 #x10 #xF0 #x80 #xF0 ; 2
          #xF0 #x10 #xF0 #x10 #xF0 ; 3
          #x90 #x90 #xF0 #x10 #x10 ; 4
          #xF0 #x80 #xF0 #x10 #xF0 ; 5
          #xF0 #x80 #xF0 #x90 #xF0 ; 6
          #xF0 #x10 #x20 #x40 #x40 ; 7
          #xF0 #x90 #xF0 #x90 #xF0 ; 8
          #xF0 #x90 #xF0 #x10 #xF0 ; 9
          #xF0 #x90 #xF0 #x90 #x90 ; A
          #xE0 #x90 #xE0 #x90 #xE0 ; B
          #xF0 #x80 #x80 #x80 #xF0 ; C
          #xE0 #x90 #x90 #x90 #xE0 ; D
          #xF0 #x80 #xF0 #x80 #xF0 ; E
          #xF0 #x80 #xF0 #x80 #x80 ; F
          #XFF #XFF #XC3 #XC3 #XC3 #XC3 #XC3 #XC3 #XFF #XFF ; 0
          #X0C #X0C #X3C #X3C #X0C #X0C #X0C #X0C #X3F #X3F ; 1
          #XFF #XFF #X03 #X03 #XFF #XFF #XC0 #XC0 #XFF #XFF ; 2
          #XFF #XFF #X03 #X03 #XFF #XFF #X03 #X03 #XFF #XFF ; 3
          #XC3 #XC3 #XC3 #XC3 #XFF #XFF #X03 #X03 #X03 #X03 ; 4
          #XFF #XFF #XC0 #XC0 #XFF #XFF #X03 #X03 #XFF #XFF ; 5
          #XFF #XFF #XC0 #XC0 #XFF #XFF #XC3 #XC3 #XFF #XFF ; 6
          #XFF #XFF #X03 #X03 #X0C #X0C #X30 #X30 #X30 #X30 ; 7
          #XFF #XFF #XC3 #XC3 #XFF #XFF #XC3 #XC3 #XFF #XFF ; 8
          #XFF #XFF #XC3 #XC3 #XFF #XFF #X03 #X03 #XFF #XFF ; 9
          #XFF #XFF #XC3 #XC3 #XFF #XFF #XC3 #XC3 #XC3 #XC3 ; A
          #XFC #XFC #XC3 #XC3 #XFC #XFC #XC3 #XC3 #XFC #XFC ; B
          #XFF #XFF #XC0 #XC0 #XC0 #XC0 #XC0 #XC0 #XFF #XFF ; C
          #XFC #XFC #XC3 #XC3 #XC3 #XC3 #XC3 #XC3 #XFC #XFC ; D
          #XFF #XFF #XC0 #XC0 #XFF #XFF #XC0 #XC0 #XFF #XFF ; E
          #XFF #XFF #XC0 #XC0 #XFF #XFF #XC0 #XC0 #XC0 #XC0 ; F
   ))

(defvar chip8-state nil)
(defvar chip8-extended-p nil)
(defvar chip8-state-pending-reg nil)
(defvar chip8-key-state (make-vector 16 0))

(defconst chip8-keys-hex "0123456789abcdef")
(defconst chip8-keys-qwerty "x123qweasdzc4rfv")
(defconst chip8-keys-qwertz "x123qweasdyc4rfv")
(defconst chip8-keys-azerty "x123azeqsdwc4rfv")

(defcustom chip8-keys chip8-keys-qwerty
  "Sequence to use for hexadecimal CHIP-8 keypad.
The string corresponds to the keys triggering the codes 0 to 9
followed by a to f."
  :type `(choice
          (const :tag "Hex" ,chip8-keys-hex)
          (const :tag "QWERTY" ,chip8-keys-qwerty)
          (const :tag "QWERTZ" ,chip8-keys-qwertz)
          (const :tag "AZERTY" ,chip8-keys-azerty)
          string)
  :group 'chip8)

(defcustom chip8-key-timeout 0.1
  "Number of seconds a key is considered pressed after key down."
  :type 'float
  :group 'chip8)

(defconst chip8-timer-interval (/ 1.0 60))
(defvar chip8-timer nil)
(defvar chip8-current-rom-path nil)

(defface chip8-black
  '((t (:foreground "white" :background "black")))
  "Face for black pixels")

(defface chip8-white
  '((t (:foreground "black" :background "white")))
  "Face for white pixels")

(defconst chip8-black-pixel (propertize "  " 'face 'chip8-black))
(defconst chip8-white-pixel (propertize "  " 'face 'chip8-white))

(defcustom chip8-beep-start-function (lambda () (message "beep"))
  "Function called when the emulator starts beeping."
  :type 'function
  :group 'chip8)

(defcustom chip8-beep-stop-function (lambda () (message nil))
  "Function called when the emulator stops beeping."
  :type 'function
  :group 'chip8)

(define-derived-mode chip8-mode special-mode "CHIP-8"
  "CHIP-8 emulator"
  (buffer-disable-undo))

(defmacro chip8-enum (&rest names)
  (let ((i 0)
        defs)
    (while names
      (push `(defconst ,(intern (format "chip8-%s" (pop names))) ,i) defs)
      (setq i (1+ i)))
    `(progn ,@ (nreverse defs))))

(chip8-enum V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 VA VB VC VD VE VF I PC SP DT ST)

(defun chip8-log (fmt &rest args)
  (when chip8-debug
    (with-current-buffer (get-buffer-create chip8-debug-buffer)
      (goto-char (point-max))
      (insert (apply 'format (concat fmt "\n") args)))))

(defun chip8--memcpy (dest dest-offset src src-offset n)
  (dotimes (i n)
    (aset dest (+ dest-offset i) (aref src (+ src-offset i)))))

(defun chip8-load-sprites ()
  (chip8--memcpy chip8-ram 0 chip8-sprites 0 (length chip8-sprites)))

(defun chip8-init-keys ()
  (dotimes (i 16)
    (define-key chip8-mode-map (string (aref chip8-keys i)) 'chip8-handle-key)))

(defun chip8-handle-key (keys)
  (interactive (list (this-command-keys)))
  (let ((key (string-match-p keys chip8-keys)))
    (when (not key)
      (user-error "unknown key"))
    (chip8-log "Pressed key %X" key)
    (cond
     ((eq chip8-state 'playing)
      (aset chip8-key-state key (float-time)))
     ((eq chip8-state 'waiting)
      (aset chip8-regs chip8-state-pending-reg key)
      (setq chip8-state-pending-reg nil)
      (setq chip8-state 'playing)
      (message "playing!")))))

(defun chip8-init-timer ()
  (when chip8-timer
    (cancel-timer chip8-timer)
    (setq chip8-timer nil))
  (when (not chip8-timer)
    (setq chip8-timer (run-with-timer 0 chip8-timer-interval 'chip8-cycle))))

(defun chip8-init ()
  (random t)
  (chip8-init-keys)
  (chip8-init-timer)
  (fillarray chip8-key-state 0)
  (fillarray chip8-regs 0)
  (aset chip8-regs chip8-PC chip8-program-start)
  (fillarray chip8-ram 0)
  (chip8-load-sprites)
  (fillarray chip8-stack 0)
  (fillarray chip8-RPL-flags 0)
  (fillarray chip8-fb 0)
  (fillarray chip8-old-fb -1)
  (fillarray chip8-xfb 0)
  (fillarray chip8-old-xfb -1))

(defun chip8-load-rom (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (let ((address chip8-program-start))
      (while (not (eobp))
        (aset chip8-ram address (char-after (point)))
        (setq address (1+ address))
        (forward-char 1)))))

(defun chip8-bump-PC ()
  (aset chip8-regs chip8-PC (+ (aref chip8-regs chip8-PC) 2)))

(defun chip8-step ()
  (let* ((PC (aref chip8-regs chip8-PC))
         (b1 (aref chip8-ram PC))
         (b2 (aref chip8-ram (1+ PC)))
         (instruction (logior (ash b1 8) b2)))
    (chip8-log "PC: %03X" (aref chip8-regs chip8-PC))
    (chip8-bump-PC)
    (chip8-exec instruction)))

(defun chip8-exec (instruction)
  (let ((type (ash (logand #xF000 instruction) -12))
        (x (ash (logand #x0F00 instruction) -8))
        (y (ash (logand #x00F0 instruction) -4))
        (nnn (logand #x0FFF instruction))
        (kk (logand #x00FF instruction))
        (n (logand #x000F instruction)))
    (cond
     ((= type #x0)
      (cond
       ((and (= x #x0) (= y #xC))
        (chip8-log "SCD %02X" n)
        (let* ((height (if chip8-extended-p chip8-xfb-height chip8-fb-height))
               (width (if chip8-extended-p chip8-xfb-width chip8-fb-width))
               (fb (if chip8-extended-p chip8-xfb chip8-fb))
               (step (if chip8-extended-p n (max (/ n 2) 1)))
               (offset (* width step)))
          (dotimes (row height)
            (dotimes (col width)
              (let ((i (+ (* (- height row 1) width) col)))
                (if (< row (- height step))
                    (aset fb i (aref fb (- i offset)))
                  (aset fb i 0))))))
        (setq chip8-fb-dirty-p t))
       ((= instruction #x00E0)
        (chip8-log "CLS")
        (let ((fb (if chip8-extended-p chip8-xfb chip8-fb))
              (old-fb (if chip8-extended-p chip8-old-xfb chip8-old-fb)))
          (fillarray fb 0)
          (fillarray old-fb -1))
        (chip8-clear-fb)
        (chip8-redraw-fb))
       ((= instruction #x00EE)
        (chip8-log "RET")
        (aset chip8-regs chip8-SP (1- (aref chip8-regs chip8-SP)))
        (aset chip8-regs chip8-PC
              (aref chip8-stack (aref chip8-regs chip8-SP))))
       ((= instruction #x00FB)
        (chip8-log "SCR")
        (let ((height (if chip8-extended-p chip8-xfb-height chip8-fb-height))
              (width (if chip8-extended-p chip8-xfb-width chip8-fb-width))
              (fb (if chip8-extended-p chip8-xfb chip8-fb))
              (step (if chip8-extended-p 4 2)))
          (dotimes (row height)
            (dotimes (col width)
              (let ((i (+ (* row width) (- width col 1))))
                (if (< col (- width step))
                    (aset fb i (aref fb (- i step)))
                  (aset fb i 0))))))
        (setq chip8-fb-dirty-p t))
       ((= instruction #x00FC)
        (chip8-log "SCL")
        (let ((height (if chip8-extended-p chip8-xfb-height chip8-fb-height))
              (width (if chip8-extended-p chip8-xfb-width chip8-fb-width))
              (fb (if chip8-extended-p chip8-xfb chip8-fb))
              (step (if chip8-extended-p 4 2)))
          (dotimes (row height)
            (dotimes (col width)
              (let ((i (+ (* row width) col)))
                (if (< col (- width step))
                    (aset fb i (aref fb (+ i step)))
                  (aset fb i 0))))))
        (setq chip8-fb-dirty-p t))
       ((= instruction #x00FD)
        (chip8-log "EXIT")
        (setq chip8-state 'stopped)
        (when chip8-timer
          (cancel-timer chip8-timer)
          (setq chip8-timer nil))
        (message "interpreter halted"))
       ((= instruction #x00FE)
        (chip8-log "LOW")
        (setq chip8-extended-p nil)
        (fillarray chip8-fb 0)
        (fillarray chip8-old-fb -1)
        (chip8-clear-fb)
        (chip8-redraw-fb))
       ((= instruction #x00FF)
        (chip8-log "HIGH")
        (setq chip8-extended-p t)
        (fillarray chip8-xfb 0)
        (fillarray chip8-old-xfb -1)
        (chip8-clear-fb)
        (chip8-redraw-fb))
       (t
        (chip8-log "SYS 0x%03X" nnn))))
     ((= type #x1)
      (chip8-log "JP 0x%03X" nnn)
      (aset chip8-regs chip8-PC nnn))
     ((= type #x2)
      (chip8-log "CALL %03X" nnn)
      (let ((SP (aref chip8-regs chip8-SP)))
        (when (>= SP (length chip8-stack))
          (error "stack overflow"))
        (aset chip8-stack SP (aref chip8-regs chip8-PC))
        (aset chip8-regs chip8-SP (1+ SP))
        (aset chip8-regs chip8-PC nnn)))
     ((= type #x3)
      (chip8-log "SE V%X, 0x%02X" x kk)
      (when (= (aref chip8-regs x) kk)
        (chip8-bump-PC)))
     ((= type #x4)
      (chip8-log "SNE V%X, 0x%02X" x kk)
      (when (/= (aref chip8-regs x) kk)
        (chip8-bump-PC)))
     ((= type #x5)
      (cond
       ((= n #x0)
        (chip8-log "SE V%X, V%X" x y)
        (when (= (aref chip8-regs x) (aref chip8-regs y))
          (chip8-bump-PC)))
       (t
        (error "unknown instruction: %04X" instruction))))
     ((= type #x6)
      (chip8-log "LD V%X, 0x%02X" x kk)
      (aset chip8-regs x kk))
     ((= type #x7)
      (chip8-log "ADD V%X, 0x%02X" x kk)
      (aset chip8-regs x (logand (+ (aref chip8-regs x) kk) #xFF)))
     ((= type #x8)
      (let ((a (aref chip8-regs x))
            (b (aref chip8-regs y)))
        (cond
         ((= n #x0)
          (chip8-log "LD V%X, V%X" x y)
          (aset chip8-regs x b))
         ((= n #x1)
          (chip8-log "OR V%X, V%X" x y)
          (aset chip8-regs x (logior a b)))
         ((= n #x2)
          (chip8-log "AND V%X, V%X" x y)
          (aset chip8-regs x (logand a b)))
         ((= n #x3)
          (chip8-log "XOR V%X, V%X" x y)
          (aset chip8-regs x (logxor a b)))
         ((= n #x4)
          (chip8-log "ADD V%X, V%X" x y)
          (let ((ret (+ a b)))
            (aset chip8-regs x (logand ret #xFF))
            (aset chip8-regs chip8-VF (if (> ret #xFF) 1 0))))
         ((= n #x5)
          (chip8-log "SUB V%X, V%X" x y)
          (let ((ret (- a b)))
            (aset chip8-regs x (logand ret #xFF))
            (aset chip8-regs chip8-VF (if (>= ret 0) 1 0))))
         ((= n #x6)
          (chip8-log "SHR V%X, V%X" x y)
          (let* ((val (if chip8-ignore-second-shift-arg a b))
                 (ret (ash val -1))
                 (lsb (logand val 1)))
            (aset chip8-regs x ret)
            (aset chip8-regs chip8-VF lsb)))
         ((= n #x7)
          (chip8-log "SUBN V%X, V%X" x y)
          (let ((ret (- b a)))
            (aset chip8-regs x (logand ret #xFF))
            (aset chip8-regs chip8-VF (if (>= ret 0) 1 0))))
         ((= n #xE)
          (chip8-log "SHL V%X, V%X" x y)
          (let* ((val (if chip8-ignore-second-shift-arg a b))
                 (ret (logand #xFF (ash val 1)))
                 (msb (ash (logand val 128) -7)))
            (aset chip8-regs x ret)
            (aset chip8-regs chip8-VF msb)))
         (t
          (error "unknown instruction: %04X" instruction)))))
     ((= type #x9)
      (cond
       ((= n #x0)
        (chip8-log "SNE V%X, V%X" x y)
        (when (/= (aref chip8-regs x) (aref chip8-regs y))
          (chip8-bump-PC)))
       (t
        (error "unknown instruction: %04X" instruction))))
     ((= type #xA)
      (chip8-log "LD I, 0x%03X" nnn)
      (aset chip8-regs chip8-I nnn))
     ((= type #xB)
      (chip8-log "JP V0, 0x%03X" nnn)
      (let ((V0 (aref chip8-regs chip8-V0)))
        (aset chip8-regs chip8-PC (+ V0 nnn))))
     ((= type #xC)
      (chip8-log "RND V%X, 0x%02X" x kk)
      (let ((rnd (random #xFF)))
        (aset chip8-regs x (logand rnd kk))))
     ((= type #xD)
      (chip8-log "DRW V%X, V%X, %X" x y n)
      (let ((height (if chip8-extended-p chip8-xfb-height chip8-fb-height))
            (width (if chip8-extended-p chip8-xfb-width chip8-fb-width))
            (fb (if chip8-extended-p chip8-xfb chip8-fb))
            (col (aref chip8-regs x))
            (row (aref chip8-regs y))
            (I (aref chip8-regs chip8-I))
            (extendedp (and chip8-extended-p (zerop n)))
            collisionp)
        (dotimes (row-offset (if extendedp 16 n))
          (dotimes (col-offset (if extendedp 16 8))
            (let ((byte (aref chip8-ram (+ I
                                           (* row-offset (if extendedp 2 1))
                                           (/ col-offset 8)))))
              (let ((r (+ row row-offset))
                    (c (+ col col-offset)))
                (when (and (< r height) (< c width))
                  (let ((power (ash 1 (- 7 (mod col-offset 8))))
                        (idx (+ (* width r) c)))
                    (when (= (logand byte power) power)
                      (when (= (aref fb idx) 1)
                        (setq collisionp t))
                      (aset fb idx (logxor (aref fb idx) 1)))))))))
        (aset chip8-regs chip8-VF (if collisionp 1 0))
        (setq chip8-fb-dirty-p t)))
     ((= type #xE)
      (let ((key (aref chip8-regs x)))
        (cond
         ((= kk #x9E)
          (chip8-log "SKP V%X" x)
          (when (< (- (float-time) (aref chip8-key-state key))
                   chip8-key-timeout)
            (chip8-bump-PC)))
         ((= kk #xA1)
          (chip8-log "SKNP V%X" x)
          (when (> (- (float-time) (aref chip8-key-state key))
                   chip8-key-timeout)
            (chip8-bump-PC)))
         (t
          (error "unknown instruction: %04X" instruction)))))
     ((= type #xF)
      (cond
       ((= kk #x07)
        (chip8-log "LD V%X, DT" x)
        (let ((DT (aref chip8-regs chip8-DT)))
          (aset chip8-regs x DT)))
       ((= kk #x0A)
        (chip8-log "LD V%X, K" x)
        (setq chip8-state 'waiting)
        (setq chip8-state-pending-reg x)
        (chip8-redraw-fb)
        (message "press key to continue"))
       ((= kk #x15)
        (chip8-log "LD DT, V%X" x)
        (aset chip8-regs chip8-DT (aref chip8-regs x)))
       ((= kk #x18)
        (chip8-log "LD ST, V%X" x)
        (aset chip8-regs chip8-ST (aref chip8-regs x))
        (when (> (aref chip8-regs chip8-ST) 0)
          (funcall chip8-beep-start-function)))
       ((= kk #x1E)
        (chip8-log "ADD I, V%X" x)
        (let* ((I (aref chip8-regs chip8-I))
               (ret (+ I (aref chip8-regs x))))
          (aset chip8-regs chip8-I (logand ret #xFFF))
          (aset chip8-regs chip8-VF (if (> ret #xFFF) 1 0))))
       ((= kk #x29)
        (chip8-log "LD F, V%X" x)
        (let ((n (aref chip8-regs x)))
          (aset chip8-regs chip8-I (* n 5))))
       ((= kk #x30)
        (chip8-log "LD HF, V%X" x)
        (let ((n (aref chip8-regs x)))
          (aset chip8-regs chip8-I (+ (* n 10) 80))))
       ((= kk #x33)
        (chip8-log "LD B, V%X" x)
        (let ((n (aref chip8-regs x))
              (I (aref chip8-regs chip8-I)))
          (aset chip8-ram I (/ n 100))
          (aset chip8-ram (+ I 1) (/ (mod n 100) 10))
          (aset chip8-ram (+ I 2) (mod n 10))))
       ((= kk #x55)
        (chip8-log "LD [I], V%X" x)
        (let ((I (aref chip8-regs chip8-I)))
          (chip8--memcpy chip8-ram I chip8-regs 0 (1+ x))))
       ((= kk #x65)
        (chip8-log "LD V%X, [I]" x)
        (let ((I (aref chip8-regs chip8-I)))
          (chip8--memcpy chip8-regs 0 chip8-ram I (1+ x))))
       ((= kk #x75)
        (chip8-log "LD R, V%X" x)
        (chip8--memcpy chip8-RPL-flags 0 chip8-regs 0 (1+ x)))
       ((= kk #x85)
        (chip8-log "LD V%X, R" x)
        (chip8--memcpy chip8-regs 0 chip8-RPL-flags 0 (1+ x)))
       (t
        (error "unknown instruction: %04X" instruction))))
     (t
      (error "unknown instruction: %04X" instruction)))))

(defun chip8-clear-fb ()
  (with-current-buffer (get-buffer-create chip8-buffer)
    (let ((height (if chip8-extended-p chip8-xfb-height chip8-fb-height))
          (width (if chip8-extended-p chip8-xfb-width chip8-fb-width))
          (buffer-read-only nil))
      (erase-buffer)
      (dotimes (_ height)
        (dotimes (_ width)
          (insert chip8-black-pixel))
        (insert "\n")))))

(defun chip8-draw-fb ()
  (with-current-buffer (get-buffer-create chip8-buffer)
    (let ((height (if chip8-extended-p chip8-xfb-height chip8-fb-height))
          (width (if chip8-extended-p chip8-xfb-width chip8-fb-width))
          (fb (if chip8-extended-p chip8-xfb chip8-fb))
          (old-fb (if chip8-extended-p chip8-old-xfb chip8-old-fb))
          (buffer-read-only nil))
      (goto-char (point-min))
      (dotimes (row height)
        (dotimes (col width)
          (let* ((idx (+ (* row width) col))
                 (pixel (aref fb idx)))
            (when (/= pixel (aref old-fb idx))
              (let ((face (if (zerop pixel) 'chip8-black 'chip8-white)))
                (put-text-property (point) (+ (point) 2) 'face face))))
          (forward-char 2))
        (forward-char 1)))))

(defun chip8-redraw-fb ()
  (chip8-draw-fb)
  (let ((old-fb (if chip8-extended-p chip8-xfb chip8-fb))
        (fb (if chip8-extended-p chip8-xfb chip8-fb))
        (size (if chip8-extended-p chip8-xfb-size chip8-fb-size)))
    (chip8--memcpy old-fb 0 fb 0 size)))

(defun chip8-cycle ()
  ;; HACK: `chip8-step' may change `chip8-state' when waiting for user
  ;; input, so we need to check on each iteration
  (dotimes (_ chip8-speed-factor)
    (when (eq chip8-state 'playing)
      (chip8-step)))
  (when (eq chip8-state 'playing)
    (let ((DT (aref chip8-regs chip8-DT))
          (ST (aref chip8-regs chip8-ST)))
      (when (> DT 0)
        (aset chip8-regs chip8-DT (1- DT)))
      (when (> ST 0)
        (aset chip8-regs chip8-ST (1- ST))
        (when (zerop (aref chip8-regs chip8-ST))
          (funcall chip8-beep-stop-function))))
    (when chip8-fb-dirty-p
      (chip8-redraw-fb)
      (setq chip8-fb-dirty-p nil))))

(defun chip8-play ()
  (setq chip8-state 'playing)
  (message "playing!"))

(defun chip8-pause ()
  (setq chip8-state 'paused)
  (message "paused"))

(defun chip8-toggle-play-pause ()
  (interactive)
  (cond
   ((eq chip8-state 'playing)
    (chip8-pause))
   ((eq chip8-state 'paused)
    (chip8-play))))

(defun chip8-reset ()
  (interactive)
  (chip8-init)
  (chip8-load-rom chip8-current-rom-path)
  (chip8-play)
  (message "reset"))

(defun chip8-quit-window (arg)
  (interactive "P")
  (quit-window arg)
  (when (eq chip8-state 'playing)
    (setq chip8-state 'paused)
    (message "automatically paused")))

(define-key chip8-mode-map (kbd "p") 'chip8-toggle-play-pause)
(define-key chip8-mode-map (kbd "g") 'chip8-reset)
(define-key chip8-mode-map (kbd "q") 'chip8-quit-window)
(define-key chip8-mode-map (kbd "Q") 'chip8-quit-window)

;;;###autoload
(defun chip8-emulate (path)
  (interactive "f")
  (chip8-init)
  (setq chip8-current-rom-path path)
  (chip8-load-rom path)
  (with-current-buffer (get-buffer-create chip8-buffer)
    (chip8-mode)
    (chip8-clear-fb)
    (chip8-play))
  (switch-to-buffer (get-buffer-create chip8-buffer)))

(provide 'chip8)
;;; chip8.el ends here
