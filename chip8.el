;;; chip8.el --- A CHIP-8 Emulator  -*- firestarter: (byte-compile-file (buffer-file-name)); -*-

;; Copyright (C) 2019 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
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

;; This is an emulator for the CHIP-8 VM which is capable of playing
;; plenty of classic games on modest machines, such as the infinitely
;; customizable text editor.  Use M-x chip8-emulate and select a ROM
;; file to be played.  You may want to customize `chip8-keys' (to
;; select a keymap fitting your keyboard), `chip8-key-timeout' (in
;; case key repeat doesn't feel right) and `chip8-speed-factor' (to
;; increase the speed for slow ROMs).

;;; Code:

(defgroup chip8 nil
  "A CHIP-8 Emulator"
  :group 'games
  :prefix "chip8-")

(defcustom chip8-ignore-second-shift-arg t
  "If non-nil, emulate a wide-spread bug in shift ops.
t: Vx = Vx SHR/SHL 1
nil: Vx = Vy SHR/SHL 1"
  :type 'boolean
  :group 'chip8)

(defconst chip8-buffer "*CHIP-8*")
(define-derived-mode chip8-mode special-mode "CHIP-8"
  "CHIP-8 emulator"
  (buffer-disable-undo))

(defvar chip8-debug nil)
(defconst chip8-debug-buffer "*CHIP-8 debug*")

(defun chip8-log (fmt &rest args)
  (when chip8-debug
    (with-current-buffer (get-buffer-create chip8-debug-buffer)
      (goto-char (point-max))
      (insert (apply 'format (concat fmt "\n") args)))))

(defvar chip8-program-start #x200)

(defun chip8-cpu-new ()
  (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ; V0-VF
          0 ; I
          chip8-program-start ; PC
          0 ; SP
          0 ; DT
          0 ; ST
          ))

(defmacro chip8-enum (&rest names)
  (let ((i 0)
        defs)
    (while names
      (push `(defconst ,(intern (format "chip8-%s" (pop names))) ,i) defs)
      (setq i (1+ i)))
    `(progn ,@ (nreverse defs))))

(chip8-enum V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 VA VB VC VD VE VF I PC SP DT ST)

(defvar chip8-regs nil)
(defvar chip8-ram nil)
(defvar chip8-stack nil)
(defvar chip8-fb nil)
(defvar chip8-old-fb nil)
(defvar chip8-fb-dirty nil)
(defconst chip8-fb-width 64)
(defconst chip8-fb-height 32)
(defconst chip8-fb-size (* chip8-fb-height chip8-fb-width))

(defun chip8--memcpy (dest dest-offset src src-offset n)
  (dotimes (i n)
    (aset dest (+ dest-offset i) (aref src (+ src-offset i)))))

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
   ))

(defun chip8-load-sprites ()
  (chip8--memcpy chip8-ram 0 chip8-sprites 0 (length chip8-sprites)))

(defconst chip8-keys-hex "0123456789abcdef")
(defconst chip8-keys-qwerty "x123qweasdzc4rfv")
(defconst chip8-keys-qwertz "x123qweasdyc4rfv")
(defconst chip8-keys-azerty "x123qweasdyc4rfv")

(defcustom chip8-keys chip8-keys-hex
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

(defvar chip8-key-state (make-vector 16 0))

(defcustom chip8-key-timeout 0.1
  "Number of seconds a key is considered pressed after key down."
  :type 'float
  :group 'chip8)

(defun chip8-init-keys ()
  (dotimes (i 16)
    (define-key chip8-mode-map (string (aref chip8-keys i)) 'chip8-handle-key)))

(defun chip8-handle-key ()
  (interactive)
  (let ((key (string-match-p (this-command-keys) chip8-keys)))
    (when (not key)
      (user-error "unknown key"))
    (chip8-log "Pressed key %X" key)
    (aset chip8-key-state key (float-time))))

(defun chip8-init ()
  (random t)
  (chip8-init-keys)
  (setq chip8-regs (chip8-cpu-new))
  (setq chip8-ram (make-vector #xFFF 0))
  (chip8-load-sprites)
  (setq chip8-stack (make-vector 16 0))
  (setq chip8-fb (make-vector chip8-fb-size 0))
  (setq chip8-old-fb (make-vector chip8-fb-size -1)))

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
        (subtype (logand #x000F instruction))
        (low-byte (logand #x00FF instruction)))
    (cond
     ((= type #x0)
      (cond
       ((= instruction #x00E0)
        (chip8-log "CLS")
        (setq chip8-fb (make-vector (* 32 64) 0))
        (setq chip8-fb-dirty t))
       ((= instruction #x00EE)
        (chip8-log "RET")
        (aset chip8-regs chip8-SP (1- (aref chip8-regs chip8-SP)))
        (aset chip8-regs chip8-PC
              (aref chip8-stack (aref chip8-regs chip8-SP))))
       (t
        (let ((addr (logand #x0FFF instruction)))
          (chip8-log "SYS 0x%03X" addr)))))
     ((= type #x1)
      (let ((addr (logand #x0FFF instruction)))
        (chip8-log "JP 0x%03X" addr)
        (aset chip8-regs chip8-PC addr)))
     ((= type #x2)
      (let ((addr (logand #x0FFF instruction))
            (SP (aref chip8-regs chip8-SP)))
        (chip8-log "CALL %03X" addr)
        (when (>= SP (length chip8-stack))
          (error "stack overflow"))
        (aset chip8-stack SP (aref chip8-regs chip8-PC))
        (aset chip8-regs chip8-SP (1+ SP))
        (aset chip8-regs chip8-PC addr)))
     ((= type #x3)
      (let ((reg (ash (logand #x0F00 instruction) -8))
            (byte (logand #x00FF instruction)))
        (chip8-log "SE V%X, 0x%02X" reg byte)
        (when (= (aref chip8-regs reg) byte)
          (chip8-bump-PC))))
     ((= type #x4)
      (let ((reg (ash (logand #x0F00 instruction) -8))
            (byte (logand #x00FF instruction)))
        (chip8-log "SNE V%X, 0x%02X" reg byte)
        (when (/= (aref chip8-regs reg) byte)
          (chip8-bump-PC))))
     ((= type #x5)
      (cond
       ((= subtype #x0)
        (let ((reg1 (ash (logand #x0F00 instruction) -8))
              (reg2 (ash (logand #x00F0 instruction) -4)))
          (chip8-log "SE V%X, V%X" reg1 reg2)
          (when (= (aref chip8-regs reg1) (aref chip8-regs reg2))
            (chip8-bump-PC))))
       (t
        (error "unknown instruction: %04X" instruction))))
     ((= type #x6)
      (let ((reg (ash (logand #x0F00 instruction) -8))
            (byte (logand #x00FF instruction)))
        (chip8-log "LD V%X, 0x%02X" reg byte)
        (aset chip8-regs reg byte)))
     ((= type #x7)
      (let ((reg (ash (logand #x0F00 instruction) -8))
            (byte (logand #x00FF instruction)))
        (chip8-log "ADD V%X, 0x%02X" reg byte)
        (aset chip8-regs reg (mod (+ (aref chip8-regs reg) byte) 256))))
     ((= type #x8)
      (let* ((reg1 (ash (logand #x0F00 instruction) -8))
             (x (aref chip8-regs reg1))
             (reg2 (ash (logand #x00F0 instruction) -4))
             (y (aref chip8-regs reg2)))
        (cond
         ((= subtype #x0)
          (chip8-log "LD V%X, V%X" reg1 reg2)
          (aset chip8-regs reg1 y))
         ((= subtype #x1)
          (chip8-log "OR V%X, V%X" reg1 reg2)
          (aset chip8-regs reg1 (logior x y)))
         ((= subtype #x2)
          (chip8-log "AND V%X, V%X" reg1 reg2)
          (aset chip8-regs reg1 (logand x y)))
         ((= subtype #x3)
          (chip8-log "XOR V%X, V%X" reg1 reg2)
          (aset chip8-regs reg1 (logxor x y)))
         ((= subtype #x4)
          (let ((ret (+ x y)))
            (chip8-log "ADD V%X, V%X" reg1 reg2)
            (aset chip8-regs reg1 (mod ret 256))
            (aset chip8-regs chip8-VF (if (> ret #xFF) 1 0))))
         ((= subtype #x5)
          (let ((ret (- x y)))
            (chip8-log "SUB V%X, V%X" reg1 reg2)
            (aset chip8-regs reg1 (mod ret 256))
            (aset chip8-regs chip8-VF (if (>= ret 0) 1 0))))
         ((= subtype #x6)
          (chip8-log "SHR V%X, V%X" reg1 reg2)
          (let* ((val (if chip8-ignore-second-shift-arg x y))
                 (ret (ash val -1))
                 (lsb (logand val 1)))
            (aset chip8-regs reg1 ret)
            (aset chip8-regs chip8-VF lsb)))
         ((= subtype #x7)
          (let ((ret (- y x)))
            (chip8-log "SUBN V%X, V%X" reg1 reg2)
            (aset chip8-regs reg1 (mod ret 256))
            (aset chip8-regs chip8-VF (if (>= ret 0) 1 0))))
         ((= subtype #xE)
          (chip8-log "SHL V%X, V%X" reg1 reg2)
          (let* ((val (if chip8-ignore-second-shift-arg x y))
                 (ret (logand #xFF (ash val 1)))
                 (msb (ash (logand val 128) -7)))
            (aset chip8-regs reg1 ret)
            (aset chip8-regs chip8-VF msb)))
         (t
          (error "unknown instruction: %04X" instruction)))))
     ((= type #x9)
      (cond
       ((= subtype #x0)
        (let ((reg1 (ash (logand #x0F00 instruction) -8))
              (reg2 (ash (logand #x00F0 instruction) -4)))
          (chip8-log "SNE V%X, V%X" reg1 reg2)
          (when (/= (aref chip8-regs reg1) (aref chip8-regs reg2))
            (chip8-bump-PC))))
       (t
        (error "unknown instruction: %04X" instruction))))
     ((= type #xA)
      (let ((addr (logand #x0FFF instruction)))
        (chip8-log "LD I, 0x%03X" addr)
        (aset chip8-regs chip8-I addr)))
     ((= type #xB)
      (let ((V0 (aref chip8-regs chip8-V0))
            (addr (logand #x0FFF instruction)))
        (chip8-log "JP V0, 0x%03X" addr)
        (aset chip8-regs chip8-PC (+ V0 addr))))
     ((= type #xC)
      (let ((reg (ash (logand #x0F00 instruction) -8))
            (byte (logand #x00FF instruction))
            (rnd (random #xFF)))
        (chip8-log "RND V%X, 0x%02X" reg byte)
        (aset chip8-regs reg (logand rnd byte))))
     ((= type #xD)
      (let* ((reg1 (ash (logand #x0F00 instruction) -8))
             (reg2 (ash (logand #x00F0 instruction) -4))
             (nibble (logand #x000F instruction))
             (x (aref chip8-regs reg1))
             (y (aref chip8-regs reg2))
             (I (aref chip8-regs chip8-I))
             collisionp)
        (chip8-log "DRW V%X, V%X, %X" reg1 reg2 nibble)
        (dotimes (yo nibble)
          (let ((byte (aref chip8-ram (+ I yo))))
            (dotimes (xo 8)
              (let ((power (ash 1 (- 7 xo)))
                    (idx (+ (* chip8-fb-width
                               (mod (+ y yo) chip8-fb-height))
                            (mod (+ x xo) chip8-fb-width))))
                (when (= (logand byte power) power)
                  (when (= (aref chip8-fb idx) 1)
                    (setq collisionp t))
                  (aset chip8-fb idx
                        (logxor (aref chip8-fb idx) 1)))))))
        (aset chip8-regs chip8-VF (if collisionp 1 0))
        (setq chip8-fb-dirty t)))
     ((= type #xE)
      (let* ((reg (ash (logand #x0F00 instruction) -8))
             (key (aref chip8-regs reg)))
        (cond
         ((= low-byte #x9E)
          (chip8-log "SKP V%X" reg)
          (when (< (- (float-time) (aref chip8-key-state key))
                   chip8-key-timeout)
            (chip8-bump-PC)))
         ((= low-byte #xA1)
          (chip8-log "SKNP V%X" reg)
          (when (> (- (float-time) (aref chip8-key-state key))
                   chip8-key-timeout)
            (chip8-bump-PC)))
         (t
          (error "unknown instruction: %04X" instruction)))))
     ((= type #xF)
      (let ((reg (ash (logand #x0F00 instruction) -8)))
        (cond
         ((= low-byte #x07)
          (let ((DT (aref chip8-regs chip8-DT)))
            (chip8-log "LD V%X, DT" reg)
            (aset chip8-regs reg DT)))
         ((= low-byte #x0A)
          (error "unimplemented: LD V%X, K" reg))
         ((= low-byte #x15)
          (chip8-log "LD DT, V%X" reg)
          (aset chip8-regs chip8-DT (aref chip8-regs reg)))
         ((= low-byte #x18)
          (chip8-log "LD ST, V%X" reg)
          (aset chip8-regs chip8-ST (aref chip8-regs reg)))
         ((= low-byte #x1E)
          (let ((I (aref chip8-regs chip8-I)))
            (chip8-log "ADD I, V%X" reg)
            ;; TODO: what if I overflows?
            (aset chip8-regs chip8-I (+ I (aref chip8-regs reg)))))
         ((= low-byte #x29)
          (let ((n (aref chip8-regs reg)))
            (chip8-log "LD F, V%X" reg)
            (aset chip8-regs chip8-I (* n 5))))
         ((= low-byte #x33)
          (let ((n (aref chip8-regs reg))
                (I (aref chip8-regs chip8-I)))
            (chip8-log "LD B, V%X" reg)
            (aset chip8-ram I (/ n 100))
            (aset chip8-ram (+ I 1) (/ (mod n 100) 10))
            (aset chip8-ram (+ I 2) (mod n 10))))
         ((= low-byte #x55)
          (let ((I (aref chip8-regs chip8-I)))
            (chip8-log "LD [I], V%X" reg)
            (chip8--memcpy chip8-ram I chip8-regs 0 (1+ reg))))
         ((= low-byte #x65)
          (let ((I (aref chip8-regs chip8-I)))
            (chip8-log "LD V%X, [I]" reg)
            (chip8--memcpy chip8-regs 0 chip8-ram I (1+ reg))))
         (t
          (error "unknown instruction: %04X" instruction)))))
     (t
      (error "unknown instruction: %04X" instruction)))))

(defconst chip8-timer-interval (/ 1.0 60))
(defvar chip8-timer nil)
(defvar chip8-state nil)
(defvar chip8-current-rom-path nil)

(defface chip8-black
  '((t (:foreground "white" :background "black")))
  "Face for black pixels")

(defface chip8-white
  '((t (:foreground "black" :background "white")))
  "Face for white pixels")

(defconst chip8-black-pixel (propertize "  " 'face 'chip8-black))
(defconst chip8-white-pixel (propertize "  " 'face 'chip8-white))

(defun chip8-clear-fb ()
  (with-current-buffer (get-buffer-create chip8-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (dotimes (_ chip8-fb-height)
        (dotimes (_ chip8-fb-width)
          (insert chip8-black-pixel))
        (insert "\n")))))

(defun chip8-draw-fb ()
  (with-current-buffer (get-buffer-create chip8-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (dotimes (row chip8-fb-height)
        (dotimes (col chip8-fb-width)
          (let* ((idx (+ (* row chip8-fb-width) col))
                 (pixel (aref chip8-fb idx)))
            (if (= pixel (aref chip8-old-fb idx))
                (forward-char 2)
              (delete-char 2)
              (insert (if (zerop pixel) chip8-black-pixel chip8-white-pixel)))))
        (forward-char 1)))))

(defcustom chip8-speed-factor 5
  "Amount of cycles to execute on each timer run.
As the timer runs at 60hz, factor 1 corresponds to 60 cps, factor
2 to 120 cps, etc."
  :type 'integer
  :group 'chip8)

(defun chip8-beep ()
  ;; TODO: actually beep
  (let ((visible-bell t))
    (ding)))

(defun chip8-cycle ()
  (when (eq chip8-state 'playing)
    (dotimes (_ chip8-speed-factor)
      (chip8-step))
    (let ((DT (aref chip8-regs chip8-DT))
          (ST (aref chip8-regs chip8-ST)))
      (when (> DT 0)
        (aset chip8-regs chip8-DT (1- DT)))
      (when (> ST 0)
        (chip8-beep)
        (aset chip8-regs chip8-ST (1- ST))))
    (when chip8-fb-dirty
      (chip8-draw-fb)
      (chip8--memcpy chip8-old-fb 0 chip8-fb 0 chip8-fb-size)
      (setq chip8-fb-dirty nil))))

(defun chip8-play ()
  (when (not chip8-timer)
    (setq chip8-timer (run-with-timer 0 chip8-timer-interval 'chip8-cycle)))
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
