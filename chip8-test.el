;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'chip8)

(defun chip8-load-instructions (&rest instructions)
  (let ((address chip8-program-start))
    (while instructions
      (let ((instruction (pop instructions)))
        (aset chip8-ram address (ash (logand #xFF00 instruction) -8))
        (setq address (1+ address))
        (aset chip8-ram address (logand #x00FF instruction))
        (setq address (1+ address))))))

(describe "The CHIP-8 CPU"
  (before-all
    (setq inhibit-message t))

  (before-each
    (chip8-init))

  (describe "CLS"
    (it "should clear the screen"
      (spy-on 'chip8-clear-fb)
      (spy-on 'chip8-redraw-fb)
      (chip8-load-instructions #x00E0)
      (chip8-step)
      (expect 'chip8-redraw-fb :to-have-been-called)))

  (describe "HLT"
    (it "should stop the emulator"
      (chip8-load-instructions #x00FD)
      (expect (chip8-step) :not :to-throw)
      (expect chip8-state :to-equal 'stopped)))

  (describe "EXT 0"
    (it "should exit extended mode"
      (spy-on 'chip8-clear-fb)
      (spy-on 'chip8-redraw-fb)
      (chip8-load-instructions #x00FE)
      (expect (chip8-step) :not :to-throw)
      (expect chip8-extended-p :to-equal nil)
      (expect 'chip8-redraw-fb :to-have-been-called)))

  (describe "EXT 1"
    (it "should enter extended mode"
      (spy-on 'chip8-clear-fb)
      (spy-on 'chip8-redraw-fb)
      (chip8-load-instructions #x00FF)
      (expect (chip8-step) :not :to-throw)
      (expect chip8-extended-p :to-equal t)
      (expect 'chip8-redraw-fb :to-have-been-called)))

  (describe "SCD n"
    (it "should scroll the screen"
      (chip8-load-instructions #x00CF)
      (expect (chip8-step) :not :to-throw)
      (expect chip8-fb-dirty :to-equal t)))

  (describe "SCL"
    (it "should scroll the screen"
      (chip8-load-instructions #x00FC)
      (expect (chip8-step) :not :to-throw)
      (expect chip8-fb-dirty :to-equal t)))

  (describe "SCR"
    (it "should scroll the screen"
      (chip8-load-instructions #x00FB)
      (expect (chip8-step) :not :to-throw)
      (expect chip8-fb-dirty :to-equal t)))

  (describe "SYS nnn"
    (it "should be ignored"
      (chip8-load-instructions #x0123 #x0789 #x0FFF)
      (expect (chip8-step) :not :to-throw)
      (expect (chip8-step) :not :to-throw)
      (expect (chip8-step) :not :to-throw)))

  (describe "unknown instructions"
    (it "should throw an error"
      (chip8-load-instructions #xEEEE #xFFFF)
      (expect (chip8-step) :to-throw)
      (expect (chip8-step) :to-throw)))

  (describe "CALL nnn"
    (it "should change the program counter"
      (chip8-load-instructions #x2208 #xFFFF #xFFFF #xFFFF #x00EE)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal #x208)))

    (it "should increment the stack pointer"
      (chip8-load-instructions #x2208 #xFFFF #xFFFF #xFFFF #x00EE)
      (let ((sp (aref chip8-regs chip8-SP)))
        (chip8-step)
        (expect (aref chip8-regs chip8-SP) :not :to-equal sp)
        (expect (aref chip8-regs chip8-SP) :to-equal (1+ sp)))))

  (describe "RET"
    (it "should change the program counter back"
      (chip8-load-instructions #x2208 #xFFFF #xFFFF #xFFFF #x00EE)
      (chip8-step)
      (expect (aref chip8-regs chip8-PC) :to-equal #x208)
      (chip8-step)
      (expect (aref chip8-regs chip8-PC) :to-equal #x202))

    (it "should decrement the stack pointer"
      (chip8-load-instructions #x2208 #xFFFF #xFFFF #xFFFF #x00EE)
      (let ((sp (aref chip8-regs chip8-SP)))
        (chip8-step)
        (expect (aref chip8-regs chip8-SP) :to-equal (1+ sp))
        (chip8-step)
        (expect (aref chip8-regs chip8-SP) :to-equal sp))))

  (describe "JP nnn"
    (it "should unconditionally change the program counter"
      (chip8-load-instructions #x1208 #xFFFF #xFFFF #xFFFF #x00E0)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal #x208))))

  (describe "JP V0, nnn"
    (it "should change the program counter indexed by V0"
      (aset chip8-regs chip8-V0 #x7F)
      (chip8-load-instructions #xB888)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ #x7F #x888)))))

  (describe "SE Vx, kk"
    (it "should increment the program counter by two instructions when true"
      (aset chip8-regs chip8-V9 #x99)
      (chip8-load-instructions #x3999 #xEEEE #xFFFF #x0000 #x0000)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 4))))

    (it "should increment the program counter by one instruction when false"
      (aset chip8-regs chip8-V9 #x33)
      (chip8-load-instructions #x3999 #xFFFF #xEEEE #x0000 #x0000)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 2)))))

  (describe "SNE Vx, kk"
    (it "should increment the program counter by two instructions when true"
      (aset chip8-regs chip8-V9 #x33)
      (chip8-load-instructions #x4999 #xEEEE #xFFFF #x0000 #x0000)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 4))))

    (it "should increment the program counter by one instruction when false"
      (aset chip8-regs chip8-V9 #x99)
      (chip8-load-instructions #x4999 #xFFFF #xEEEE #x0000 #x0000)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 2)))))

  (describe "SE Vx, Vy"
    (it "should increment the program counter by two instructions when true"
      (aset chip8-regs chip8-V6 #x33)
      (aset chip8-regs chip8-V9 #x33)
      (chip8-load-instructions #x5690 #xEEEE #xFFFF #x0000 #x0000)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 4))))

    (it "should increment the program counter by one instruction when false"
      (aset chip8-regs chip8-V6 #x66)
      (aset chip8-regs chip8-V9 #x33)
      (chip8-load-instructions #x5690 #xFFFF #xEEEE #x0000 #x0000)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 2)))))

  (describe "SNE Vx, Vy"
    (it "should increment the program counter by two instructions when true"
      (aset chip8-regs chip8-V6 #x66)
      (aset chip8-regs chip8-V9 #x33)
      (chip8-load-instructions #x9690 #xEEEE #xFFFF #x0000 #x0000)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 4))))

    (it "should increment the program counter by one instruction when false"
      (aset chip8-regs chip8-V6 #x33)
      (aset chip8-regs chip8-V9 #x33)
      (chip8-load-instructions #x9690 #xFFFF #xEEEE #x0000 #x0000)
      (let ((pc (aref chip8-regs chip8-PC)))
        (chip8-step)
        (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
        (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 2)))))

  (describe "SKP Vx"
    (it "should increment the program counter by two instructions when true"
      (aset chip8-regs chip8-V9 #xF)
      (chip8-load-instructions #xE99E #xEEEE #xFFFF #x0000 #x0000)
      (let ((chip8-state 'playing)
            (chip8-keys chip8-keys-hex))
        (chip8-handle-key "f")
        (let ((pc (aref chip8-regs chip8-PC)))
          (chip8-step)
          (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
          (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 4)))))

    (it "should increment the program counter by one instruction when false"
      (aset chip8-regs chip8-V9 #xF)
      (chip8-load-instructions #xE99E #xFFFF #xEEEE #x0000 #x0000)
      (let ((chip8-state 'playing)
            (chip8-keys chip8-keys-hex))
        (chip8-handle-key "e")
        (let ((pc (aref chip8-regs chip8-PC)))
          (chip8-step)
          (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
          (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 2))))))

  (describe "SKNP Vx"
    (it "should increment the program counter by two instructions when true"
      (aset chip8-regs chip8-V9 #xF)
      (chip8-load-instructions #xE9A1 #xEEEE #xFFFF #x0000 #x0000)
      (let ((chip8-state 'playing)
            (chip8-keys chip8-keys-hex))
        (chip8-handle-key "e")
        (let ((pc (aref chip8-regs chip8-PC)))
          (chip8-step)
          (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
          (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 4)))))

    (it "should increment the program counter by one instruction when false"
      (aset chip8-regs chip8-V9 #xF)
      (chip8-load-instructions #xE9A1 #xFFFF #xEEEE #x0000 #x0000)
      (let ((chip8-state 'playing)
            (chip8-keys chip8-keys-hex))
        (chip8-handle-key "f")
        (let ((pc (aref chip8-regs chip8-PC)))
          (chip8-step)
          (expect (aref chip8-regs chip8-PC) :not :to-equal pc)
          (expect (aref chip8-regs chip8-PC) :to-equal (+ pc 2))))))

  (describe "LD Vx, kk"
    (it "should set the register to the byte literal"
      (aset chip8-regs chip8-V6 #xFF)
      (chip8-load-instructions #x6633)
      (chip8-step)
      (expect (aref chip8-regs chip8-V6) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-V6) :to-equal #x33)))

  (describe "LD Vx, Vy"
    (it "should set the register to the value of the other register"
      (aset chip8-regs chip8-V6 #xFF)
      (aset chip8-regs chip8-V9 #x33)
      (chip8-load-instructions #x8690)
      (chip8-step)
      (expect (aref chip8-regs chip8-V6) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-V6) :to-equal #x33)))

  (describe "LD I, nnn"
    (it "should set the register to the address"
      (aset chip8-regs chip8-I #xFFF)
      (chip8-load-instructions #xA888)
      (chip8-step)
      (expect (aref chip8-regs chip8-I) :not :to-equal #xFFF)
      (expect (aref chip8-regs chip8-I) :to-equal #x888)))

  (describe "LD Vx, DT"
    (it "should set the register to the delay timer value"
      (aset chip8-regs chip8-V2 #x10)
      (aset chip8-regs chip8-DT #xFF)
      (chip8-load-instructions #xF207)
      (chip8-step)
      (expect (aref chip8-regs chip8-V2) :not :to-equal #x10)
      (expect (aref chip8-regs chip8-V2) :to-equal #xFF)))

  (describe "LD Vx, K"
    (it "should set the register to the pressed key"
      (spy-on 'chip8-redraw-fb)
      (aset chip8-regs chip8-V2 #x10)
      (chip8-load-instructions #xF20A)
      (chip8-step)
      (let ((chip8-keys chip8-keys-hex))
        (chip8-handle-key "f")
        (expect (aref chip8-regs chip8-V2) :not :to-equal #x10)
        (expect (aref chip8-regs chip8-V2) :to-equal #xF))))

  (describe "LD DT, Vx"
    (it "should set the delay timer to the register value"
      (aset chip8-regs chip8-V2 #x10)
      (aset chip8-regs chip8-DT #xFF)
      (chip8-load-instructions #xF215)
      (chip8-step)
      (expect (aref chip8-regs chip8-DT) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-DT) :to-equal #x10)))

  (describe "LD ST, Vx"
    (it "should set the sound timer to the register value"
      (aset chip8-regs chip8-V2 #x10)
      (aset chip8-regs chip8-ST #xFF)
      (chip8-load-instructions #xF218)
      (chip8-step)
      (expect (aref chip8-regs chip8-ST) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-ST) :to-equal #x10)))

  (describe "LD F, Vx"
    (it "should set the I register to the sprite address in the register"
      (aset chip8-regs chip8-I #x999)
      (aset chip8-regs chip8-V2 #xF)
      (chip8-load-instructions #xF229)
      (chip8-step)
      (expect (aref chip8-regs chip8-I) :not :to-equal #x999)))

  (describe "LD FF, Vx"
    (it "should set the I register to the sprite address in the register"
      (aset chip8-regs chip8-I #x999)
      (aset chip8-regs chip8-V2 #xF)
      (chip8-load-instructions #xF230)
      (chip8-step)
      (expect (aref chip8-regs chip8-I) :not :to-equal #x999)))

  (describe "LD B, Vx"
    (it "should write the decimal digits of the number in the register"
      (aset chip8-regs chip8-I #x800)
      (aset chip8-regs chip8-V5 123)
      (chip8-load-instructions #xF533)
      (chip8-step)
      (expect (aref chip8-ram #x800) :to-equal 1)
      (expect (aref chip8-ram #x801) :to-equal 2)
      (expect (aref chip8-ram #x802) :to-equal 3))

    (it "should fill in a zero for a double-digit number"
      (aset chip8-regs chip8-I #x800)
      (aset chip8-regs chip8-V5 42)
      (chip8-load-instructions #xF533)
      (chip8-step)
      (expect (aref chip8-ram #x800) :to-equal 0)
      (expect (aref chip8-ram #x801) :to-equal 4)
      (expect (aref chip8-ram #x802) :to-equal 2))

    (it "should fill in more zeroes for a single-digit number"
      (aset chip8-regs chip8-I #x800)
      (aset chip8-regs chip8-V5 7)
      (chip8-load-instructions #xF533)
      (chip8-step)
      (expect (aref chip8-ram #x800) :to-equal 0)
      (expect (aref chip8-ram #x801) :to-equal 0)
      (expect (aref chip8-ram #x802) :to-equal 7)))

  (describe "LD [I], Vx"
    (it "should store up to the specified register into memory"
      (aset chip8-regs chip8-V0 #x10)
      (aset chip8-regs chip8-V1 #x20)
      (aset chip8-regs chip8-V2 #x30)
      (aset chip8-regs chip8-I #x800)
      (chip8-load-instructions #xF255)
      (chip8-step)
      (expect (aref chip8-ram #x800) :to-equal #x10)
      (expect (aref chip8-ram #x801) :to-equal #x20)
      (expect (aref chip8-ram #x802) :to-equal #x30)))

  (describe "LD Vx, [I]"
    (it "should load up to the specified register from memory"
      (aset chip8-ram #x800 #x10)
      (aset chip8-ram #x801 #x20)
      (aset chip8-ram #x802 #x30)
      (aset chip8-regs chip8-I #x800)
      (chip8-load-instructions #xF265)
      (chip8-step)
      (expect (aref chip8-regs chip8-V0) :to-equal #x10)
      (expect (aref chip8-regs chip8-V1) :to-equal #x20)
      (expect (aref chip8-regs chip8-V2) :to-equal #x30)))

  (describe "LD [RPL], Vx"
    (it "should store up to the specified register into the RPL registers"
      (aset chip8-regs chip8-V0 #x10)
      (aset chip8-regs chip8-V1 #x20)
      (aset chip8-regs chip8-V2 #x30)
      (chip8-load-instructions #xF275)
      (chip8-step)
      (expect (aref chip8-RPL-flags 0) :to-equal #x10)
      (expect (aref chip8-RPL-flags 1) :to-equal #x20)
      (expect (aref chip8-RPL-flags 2) :to-equal #x30)))

  (describe "LD Vx, [RPL]"
    (it "should load up to the specified register from the RPL registers"
      (aset chip8-RPL-flags 0 #x10)
      (aset chip8-RPL-flags 1 #x20)
      (aset chip8-RPL-flags 2 #x30)
      (chip8-load-instructions #xF285)
      (chip8-step)
      (expect (aref chip8-regs chip8-V0) :to-equal #x10)
      (expect (aref chip8-regs chip8-V1) :to-equal #x20)
      (expect (aref chip8-regs chip8-V2) :to-equal #x30)))

  (describe "ADD Vx, kk"
    (it "should store the sum of the register and constant"
      (aset chip8-regs chip8-VA #x7F)
      (chip8-load-instructions #x7A80)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :not :to-equal #x7F)
      (expect (aref chip8-regs chip8-VA) :to-equal #xFF))

    (it "should handle overflow by wrapping around"
      (aset chip8-regs chip8-VA #xE5)
      (chip8-load-instructions #x7A80)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :not :to-equal #xE5)
      (expect (aref chip8-regs chip8-VA) :to-equal #x65)))

  (describe "ADD Vx, Vx"
    (it "should store the sum of both registers"
      (aset chip8-regs chip8-VA #x7F)
      (aset chip8-regs chip8-VB #x80)
      (chip8-load-instructions #x8AB4)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :not :to-equal #x7F)
      (expect (aref chip8-regs chip8-VA) :to-equal #xFF))

    (it "should handle overflow by wrapping around"
      (aset chip8-regs chip8-VA #xE5)
      (aset chip8-regs chip8-VB #x80)
      (chip8-load-instructions #x8AB4)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :not :to-equal #xE5)
      (expect (aref chip8-regs chip8-VA) :to-equal #x65))

    (it "should set the overflow flag to 0 when there's no overflow"
      (aset chip8-regs chip8-VA #x7F)
      (aset chip8-regs chip8-VB #x80)
      (aset chip8-regs chip8-VF #xFF)
      (chip8-load-instructions #x8AB4)
      (chip8-step)
      (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-VF) :to-equal #x00))

    (it "should set the overflow flag to 1 when there's an overflow"
      (aset chip8-regs chip8-VA #xE5)
      (aset chip8-regs chip8-VB #x80)
      (aset chip8-regs chip8-VF #xFF)
      (chip8-load-instructions #x8AB4)
      (chip8-step)
      (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-VF) :to-equal #x01)))

  (describe "ADD I, Vx"
    (it "should increment I by the register value"
      (aset chip8-regs chip8-I #x800)
      (aset chip8-regs chip8-VA #x10)
      (chip8-load-instructions #xFA1E)
      (chip8-step)
      (expect (aref chip8-regs chip8-I) :not :to-equal #x800)
      (expect (aref chip8-regs chip8-I) :to-equal #x810))

    (it "should handle overflow by wrapping around"
      (aset chip8-regs chip8-I #xFF0)
      (aset chip8-regs chip8-VA #x10)
      (chip8-load-instructions #xFA1E)
      (chip8-step)
      (expect (aref chip8-regs chip8-I) :not :to-equal #xFF0)
      (expect (aref chip8-regs chip8-I) :to-equal #x000))

    (it "should set the overflow flag to 0 when there's no overflow"
      (aset chip8-regs chip8-I #x800)
      (aset chip8-regs chip8-VA #x10)
      (aset chip8-regs chip8-VF #xFF)
      (chip8-load-instructions #xFA1E)
      (chip8-step)
      (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-VF) :to-equal #x00))

    (it "should set the overflow flag to 1 when there's an overflow"
      (aset chip8-regs chip8-I #xFF0)
      (aset chip8-regs chip8-VA #x10)
      (aset chip8-regs chip8-VF #xFF)
      (chip8-load-instructions #xFA1E)
      (chip8-step)
      (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-VF) :to-equal #x01)))

  (describe "SUB Vx, Vy"
    (it "should store the difference between both registers"
      (aset chip8-regs chip8-VA #x30)
      (aset chip8-regs chip8-VB #x2A)
      (chip8-load-instructions #x8AB5)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :not :to-equal #x30)
      (expect (aref chip8-regs chip8-VA) :to-equal #x06))

    (it "should handle underflow by wrapping around"
      (aset chip8-regs chip8-VA #x20)
      (aset chip8-regs chip8-VB #x2A)
      (chip8-load-instructions #x8AB5)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :not :to-equal #x20)
      (expect (aref chip8-regs chip8-VA) :to-equal #xF6))

    (it "should set the overflow flag to 1 when there's no underflow"
      (aset chip8-regs chip8-VA #x30)
      (aset chip8-regs chip8-VB #x2A)
      (aset chip8-regs chip8-VF #xFF)
      (chip8-load-instructions #x8AB5)
      (chip8-step)
      (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-VF) :to-equal #x01))

    (it "should set the overflow tag to 0 when there's an underflow"
      (aset chip8-regs chip8-VA #x20)
      (aset chip8-regs chip8-VB #x2A)
      (aset chip8-regs chip8-VF #xFF)
      (chip8-load-instructions #x8AB5)
      (chip8-step)
      (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-VF) :to-equal #x00)))

  (describe "SUBN Vx, Vy"
    (it "should store the complementary difference between both registers"
      (aset chip8-regs chip8-VA #x2A)
      (aset chip8-regs chip8-VB #x30)
      (chip8-load-instructions #x8AB7)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :not :to-equal #x30)
      (expect (aref chip8-regs chip8-VA) :to-equal #x06))

    (it "should handle underflow by wrapping around"
      (aset chip8-regs chip8-VA #x2A)
      (aset chip8-regs chip8-VB #x20)
      (chip8-load-instructions #x8AB7)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :not :to-equal #x20)
      (expect (aref chip8-regs chip8-VA) :to-equal #xF6))

    (it "should set the overflow flag to 1 when there's no underflow"
      (aset chip8-regs chip8-VA #x2A)
      (aset chip8-regs chip8-VB #x30)
      (aset chip8-regs chip8-VF #xFF)
      (chip8-load-instructions #x8AB7)
      (chip8-step)
      (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-VF) :to-equal #x01))

    (it "should set the overflow tag to 0 when there's an underflow"
      (aset chip8-regs chip8-VA #x2A)
      (aset chip8-regs chip8-VB #x20)
      (aset chip8-regs chip8-VF #xFF)
      (chip8-load-instructions #x8AB7)
      (chip8-step)
      (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
      (expect (aref chip8-regs chip8-VF) :to-equal #x00)))

  (describe "AND Vx, Vy"
    (it "should store the logical AND combination of both registers"
      (aset chip8-regs chip8-VA #x65)
      (aset chip8-regs chip8-VB #x42)
      (chip8-load-instructions #x8AB2)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :to-equal (logand #x65 #x42))))

  (describe "OR Vx, Vy"
    (it "should store the logical OR combination of both registers"
      (aset chip8-regs chip8-VA #x65)
      (aset chip8-regs chip8-VB #x42)
      (chip8-load-instructions #x8AB1)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :to-equal (logior #x65 #x42))))

  (describe "XOR Vx, Vy"
    (it "should store the logical XOR combination of both registers"
      (aset chip8-regs chip8-VA #x65)
      (aset chip8-regs chip8-VB #x42)
      (chip8-load-instructions #x8AB3)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :to-equal (logxor #x65 #x42)))

    (it "should toggle individual bits"
      (aset chip8-regs chip8-VA #x65)
      (aset chip8-regs chip8-VB #xFF)
      (chip8-load-instructions #x8AB3)
      (chip8-step)
      (expect (aref chip8-regs chip8-VA) :to-equal
              (logand #xFF (lognot #x65)))))

  (describe "SHR Vx, Vy"
    (it "should store the first register shifted right in compat mode"
      (let ((chip8-ignore-second-shift-arg t))
        (aset chip8-regs chip8-VA #x81)
        (chip8-load-instructions #x8AB6)
        (chip8-step)
        (expect (aref chip8-regs chip8-VA) :not :to-equal #x81)
        (expect (aref chip8-regs chip8-VA) :to-equal #x40)))

    (it "should hold the first register's LSB in the overflow flag in compat mode"
      (let ((chip8-ignore-second-shift-arg t))
        (aset chip8-regs chip8-VA #x81)
        (aset chip8-regs chip8-VF #xFF)
        (chip8-load-instructions #x8AB6)
        (chip8-step)
        (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
        (expect (aref chip8-regs chip8-VF) :to-equal #x01)))

    (it "should otherwise store the second register shifted right"
      (let ((chip8-ignore-second-shift-arg nil))
        (aset chip8-regs chip8-VA #xFF)
        (aset chip8-regs chip8-VB #x7A)
        (chip8-load-instructions #x8AB6)
        (chip8-step)
        (expect (aref chip8-regs chip8-VA) :not :to-equal #xFF)
        (expect (aref chip8-regs chip8-VA) :to-equal #x3D)))

    (it "should otherwise hold the second register's LSB in the overflow flag"
      (let ((chip8-ignore-second-shift-arg nil))
        (aset chip8-regs chip8-VA #xFF)
        (aset chip8-regs chip8-VB #x7A)
        (aset chip8-regs chip8-VF #xFF)
        (chip8-load-instructions #x8AB6)
        (chip8-step)
        (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
        (expect (aref chip8-regs chip8-VF) :to-equal #x00))))

  (describe "SHL Vx, Vy"
    (it "should store the first register shifted left in compat mode"
      (let ((chip8-ignore-second-shift-arg t))
        (aset chip8-regs chip8-VA #x81)
        (chip8-load-instructions #x8ABE)
        (chip8-step)
        (expect (aref chip8-regs chip8-VA) :not :to-equal #x81)
        (expect (aref chip8-regs chip8-VA) :to-equal #x02)))

    (it "should hold the first register's MSB in the overflow flag in compat mode"
      (let ((chip8-ignore-second-shift-arg t))
        (aset chip8-regs chip8-VA #x81)
        (aset chip8-regs chip8-VF #xFF)
        (chip8-load-instructions #x8ABE)
        (chip8-step)
        (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
        (expect (aref chip8-regs chip8-VF) :to-equal #x01)))

    (it "should otherwise store the second register shifted left"
      (let ((chip8-ignore-second-shift-arg nil))
        (aset chip8-regs chip8-VA #xFF)
        (aset chip8-regs chip8-VB #x7A)
        (chip8-load-instructions #x8ABE)
        (chip8-step)
        (expect (aref chip8-regs chip8-VA) :not :to-equal #xFF)
        (expect (aref chip8-regs chip8-VA) :to-equal #xF4)))

    (it "should otherwise hold the second register's MSB in the overflow flag"
      (let ((chip8-ignore-second-shift-arg nil))
        (aset chip8-regs chip8-VA #xFF)
        (aset chip8-regs chip8-VB #x7A)
        (aset chip8-regs chip8-VF #xFF)
        (chip8-load-instructions #x8ABE)
        (chip8-step)
        (expect (aref chip8-regs chip8-VF) :not :to-equal #xFF)
        (expect (aref chip8-regs chip8-VF) :to-equal #x00))))

  (describe "RND Vx, kk"
    (it "should generate a random number with the full range"
      (apply 'chip8-load-instructions (make-list 100 #xC5FF))
      (dotimes (_ 100)
        (chip8-step)
        (let ((rnd (aref chip8-regs chip8-V5)))
          (expect rnd :to-be-weakly-greater-than 0)
          (expect rnd :to-be-less-than 256))))

    (it "should generate a random number with a limited range"
      (apply 'chip8-load-instructions (make-list 100 #xC507))
      (dotimes (_ 100)
        (chip8-step)
        (let ((rnd (aref chip8-regs chip8-V5)))
          (expect rnd :to-be-weakly-greater-than 0)
          (expect rnd :to-be-less-than 8)))))

  (describe "DRW Vx, Vx, n"
    (it "should not throw an error, no matter the coordinates"
      (aset chip8-regs chip8-VA #xFF)
      (aset chip8-regs chip8-VB #xFF)
      (chip8-load-instructions #xDABF)
      (expect (chip8-step) :not :to-throw)
      (expect chip8-fb-dirty :to-equal t))

    (it "should also draw in extended mode for a height of zero"
      (aset chip8-regs chip8-VA #xFF)
      (aset chip8-regs chip8-VB #xFF)
      (setq chip8-extended-p t)
      (chip8-load-instructions #xDAB0)
      (expect (chip8-step) :not :to-throw)
      (expect chip8-fb-dirty :to-equal t))))
