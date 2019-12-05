(in-package #:unicorn-test)

(defun make-bytes (&rest bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(defun dump-registers (engine)
  (dolist (reg (list :rax :rbx :rcx :rdx
                     :rsi :rdi :r8 :r9
                     :r10 :r11 :r12 :r13
                     :r14 :r15 :rsp))
    (format t "~a: ~16,'0x~%" (symbol-name reg) (read-register engine reg))))

(defcallback trace-hook :void
    ((uc :pointer)
     (address :unsigned-long-long)
     (size :unsigned-int)
     (user-data :pointer))
  (declare (ignore uc user-data))
  (cs:with-open-handle (csh :x86 :64)
    (cs:do-disassembled-instructions (insn csh (read-memory uc address size))
      (format t "0x~x: ~a ~a~%" address
              (cs:instruction-mnemonic insn)
              (cs:instruction-operand-string insn)))))

(defun test ()
  (let ((address #x1000000)
        (code (make-bytes
               #x41 #xBC #x3B #xB0
               #x28 #x2A #x49 #x0F
               #xC9 #x90 #x4D #x0F
               #xAD #xCF #x49 #x87
               #xFD #x90 #x48 #x81
               #xD2 #x8A #xCE #x77
               #x35 #x48 #xF7 #xD9
               #x4D #x29 #xF4 #x49
               #x81 #xC9 #xF6 #x8A
               #xC6 #x53 #x4D #x87
               #xED #x48 #x0F #xAD
               #xD2 #x49 #xF7 #xD4
               #x48 #xF7 #xE1 #x4D
               #x19 #xC5 #x4D #x89
               #xC5 #x48 #xF7 #xD6
               #x41 #xB8 #x4F #x8D
               #x6B #x59 #x4D #x87
               #xD0 #x68 #x6A #x1E
               #x09 #x3C #x59)))
    (with-emulator (uc :x86 :64)
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (write-register uc :rax #x71f3029efd49d41d)
      (write-register uc :rbx #xd87b45277f133ddb)
      (write-register uc :rcx #xab40d1ffd8afc461)
      (write-register uc :rdx #x919317b4a733f01)
      (write-register uc :rsi #x4c24e753a17ea358)
      (write-register uc :rdi #xe509a57d2571ce96)
      (write-register uc :r8  #xea5b108cc2b9ab1f)
      (write-register uc :r9  #x19ec097c8eb618c1)
      (write-register uc :r10 #xec45774f00c5f682)
      (write-register uc :r11 #xe17e9dbec8c074aa)
      (write-register uc :r12 #x80f86a8dc0f6d457)
      (write-register uc :r13 #x48288ca5671c5492)
      (write-register uc :r14 #x595f72f6e4017f6e)
      (write-register uc :r15 #x1efd97aea331cccc)
      (write-register uc :rsp (+ address #x200000))
      (add-hook uc :code 'trace-hook)
      (dump-registers uc)
      (start uc
             :begin address
             :until (1- (+ address (length code))))
      (dump-registers uc))))


#+nil(test)
