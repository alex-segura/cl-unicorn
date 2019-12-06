;;;; x86.lisp

(in-package #:unicorn-test)

(in-suite x86)

(defstruct basic-block
  address
  size)

(defstruct basic-block-test
  blocks
  block-number)

(defvar *basic-block-test*)

(autowrap:defcallback test-basic-blocks-hook :void
    ((uc :pointer)
     (address :unsigned-long-long)
     (size :unsigned-int)
     (user-data :pointer))
  (declare (ignore uc user-data))
  (let ((bb (nth (basic-block-test-block-number *basic-block-test*)
                 (basic-block-test-blocks *basic-block-test*))))
    (assert (and (= address (basic-block-address bb))
                 (= size (basic-block-size bb))))
    (incf (basic-block-test-block-number *basic-block-test*))))

(test test-basic-blocks
  (with-open-engine (uc :x86 :32)
    (let* ((address #x100000)
           (*basic-block-test*
             (make-basic-block-test
              :blocks
              (list (make-basic-block
                     :address address
                     :size 6)
                    (make-basic-block
                     :address (+ address 6)
                     :size 3))
              :block-number 0))
           (code (make-bytes #x33 #xc0 ; xor eax, eax
                             #x90      ; nop
                             #x90      ; nop
                             #xeb #x00 ; jmp $+2
                             #x90      ; nop
                             #x90      ; nop
                             #x90)))   ; nop
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (add-hook uc :block 'test-basic-blocks-hook :begin 1 :end 0)
      (is (null (start uc :begin address :until (+ address (length code))))
          "Basic block assertions ran without errors"))))

(autowrap:defcallback hook-block :void
    ((uc :pointer)
     (address :unsigned-long-long)
     (size :unsigned-int)
     (user-data :pointer))
  (declare (ignore uc user-data))
  (format t "~&>>> Tracing basic block at 0x~x block-size=0x~x~%" address size))

(autowrap:defcallback hook-code :void
    ((uc :pointer)
     (address :unsigned-long-long)
     (size :unsigned-int)
     (user-data :pointer))
  (declare (ignore user-data))
  (format t "~&>>> Tracing instruction at 0x~x: instruction-size=0x~x~%" address size)
  (format t "~&>>> --- EFLAGS is 0x~x~%" (read-register uc :eflags)))

(test test-i386
  (with-open-engine (uc :x86 :32)
    (let* ((code (make-bytes #x41 #x4a #x66 #x0f #xef #xc1)) ; inc ecx; dec edx; pxor xmm0, xmm1
           (address #x1000000))
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (write-register uc :ecx #x1234)
      (write-register uc :edx #x7890)
      (write-register uc :xmm0 (list #x08090a0b0c0d0e0f #x0001020304050607))
      (write-register uc :xmm1 (list #x8090a0b0c0d0e0f0 #x0010203040506070))
      (add-hook uc :block 'hook-block)
      (add-hook uc :code 'hook-code)
      (start uc :begin address :until (+ address (length code)))
      (is (= #x1235 (read-register uc :ecx)))
      (is (= #x788f (read-register uc :edx)))
      (is (equal (list #x8899aabbccddeeff #x0011223344556677)
                 (read-register uc :xmm0 :size 2)))
      (read-memory uc address 4))))

(test test-i386-jump
  (with-open-engine (uc :x86 :32)
    (let ((code (make-bytes #xeb #x02 #x90 #x90 #x90 #x90 #x90))
          (address #x1000000))
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (add-hook uc :block 'hook-block)
      (add-hook uc :code 'hook-code)
      (is (null (start uc :begin address :until (+ address (length code))))))))

(autowrap:defcallback hook-in :void
    ((uc :pointer)
     (port :unsigned-int)
     (size :int)
     (user-data :pointer))
  (declare (ignore user-data))
  (let ((eip (read-register uc :eip)))
    (format t "~&--- reading from port 0x~x, size: ~a, address: 0x~x~%"
            port size eip)
    (case size
      (1 #xf1)
      (2 #xf2)
      (4 #xf4)
      (t #x00))))

(autowrap:defcallback hook-out :void
    ((uc :pointer)
     (port :unsigned-int)
     (size :int)
     (value :unsigned-int)
     (user-data :pointer))
  (declare (ignore user-data))
  (let ((eip (read-register uc :eip))
        (tmp nil))
    (format t "~&--- writing to port 0x~x, size: ~a, value: 0x~x, address: 0x~x~%"
            port size value eip)
    (setq tmp
          (case size
            (1 (read-register uc :al))
            (2 (read-register uc :ax))
            (4 (read-register uc :eax))))
    (when tmp
      (format t "~&--- register value = 0x~x~%" tmp))))

(test test-i386-inout
  (with-open-engine (uc :x86 :32)
    (let ((address #x1000000)
          (code (make-bytes #x41 #xe4 #x3f #x4a #xe6 #x46 #x43)))
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (write-register uc :eax #x1234)
      (write-register uc :ecx #x6789)
      (add-hook uc :block 'hook-block)
      (add-hook uc :code 'hook-code)
      (add-hook uc :insn 'hook-in :instruction-id :in)
      (add-hook uc :insn 'hook-out :instruction-id :out)
      (is (null (start uc :begin address :until (+ address (length code)))))
      #+nil(is (= (read-register uc :eax) #x12f0))
      #+nil(is (= (read-register uc :ecx) #x678a)))))

(test test-i386-loop
  (let ((address #x1000000)
        (code (make-bytes #x41 #x4a #xeb #xfe)))
    (with-open-engine (uc :x86 :32)
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (write-register uc :ecx #x1234)
      (write-register uc :edx #x7890)
      (start uc :begin address
                :until (+ address (length code))
                :timeout (* 2 +second-scale+))
      (is (= (read-register uc :ecx) #x1235))
      (is (= (read-register uc :edx) #x788f)))))

(test test-i386-invalid-mem-read
  (let ((address #x1000000)
        (code (make-bytes #x8b #x0d #xaa #xaa #xaa #xaa)))
    (with-open-engine (uc :x86 :32)
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (signals unicorn-error (start uc :begin address :until (+ address (length code)))))))

(test test-i386-invalid-mem-write
  (let ((address #x1000000)
        (code (make-bytes #x89 #x0d #xaa #xaa #xaa #xaa)))
    (with-open-engine (uc :x86 :32)
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (signals unicorn-error (start uc :begin address :until (+ address (length code)))))))

(test test-i386-jump-invalid
  (let ((address #x1000000)
        (code (make-bytes #xe9 #xe9 #xee #xee #xee #xee)))
    (with-open-engine (uc :x86 :32)
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (signals unicorn-error (start uc :begin address :until (+ address (length code)))))))

(autowrap:defcallback hook-mem64 :void
    ((uc :pointer)
     (mem-type unicorn-ffi:uc-mem-type)
     (address :unsigned-long-long)
     (size :int)
     (value :long-long)
     (user-data :pointer))
  (declare (ignore uc user-data))
  (case mem-type
    (:read
     (format t "~&Memory is being READ at 0x~x, data size = ~a~%" address size))
    (:write
     (format t "~&Memory is being WRITTEN at 0x~x, data size = ~a, data value = 0x~x~%"
             address size value))))

(autowrap:defcallback hook-code64 :void
    ((uc :pointer)
     (address :unsigned-long-long)
     (size :unsigned-int)
     (user-data :pointer))
  (declare (ignore user-data))
  (let ((rip (read-register uc :rip)))
    (format t "~&>>> Tracing instruction at 0x~x, instruction size = 0x~x~%" address size)
    (format t "~&>>> RIP is 0x~x~%" rip)))

(test test-x86-64
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
    (with-open-engine (uc :x86 :64)
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
      (add-hook uc :block 'hook-block)
      (add-hook uc :code 'hook-code64 :begin address :end (+ address 20))
      (add-hook uc :mem-write 'hook-mem64)
      (is (null (start uc :begin address
                          :until (1- (+ address (length code)))))))))

(autowrap:defcallback hook-syscall :void
    ((uc :pointer)
     (user-data :pointer))
  (declare (ignore user-data))
  (let ((rax (read-register uc :rax)))
    (assert (= rax #x100) (rax) "RAX is unexpected: ~a" rax))
  (write-register uc :rax #x200))

(test test-x86-64-syscall
  (let ((address #x1000000)
        (code (make-bytes #x0f #x05)))
    (with-open-engine (uc :x86 :64)
      (map-memory uc address (* 2 1024 1024) :all)
      (write-memory uc address code)
      (add-hook uc :insn 'hook-syscall :instruction-id :syscall)
      (write-register uc :rax #x100)
      (is (null (start uc :begin address :until (+ address (length code)))))
      (is (= #x200 (read-register uc :rax))))))

(test test-x86-16
  (let ((address 0)
        (code (make-bytes #x00 #x00)))
    (with-open-engine (uc :x86 :16)
      (map-memory uc address (* 8 1024) :all)
      (write-memory uc address code)
      (write-register uc :eax 7)
      (write-register uc :ebx 5)
      (write-register uc :esi 6)
      (start uc :begin address :until (+ address (length code)))
      (is (= (aref (read-memory uc 11 1) 0) 7)))))

(test test-i386-reg-save
  (let ((code (make-bytes #x40))
        (address 0))
    (with-open-engine (uc :x86 :32)
      (map-memory uc address (* 8 1024) :all)
      (write-memory uc address code)
      (write-register uc :eax 1)
      (start uc :begin address :until (+ address 1))
      (let ((context (save-context uc)))
        (start uc :begin address :until (+ address 1))
        (is (= (read-register uc :eax) 3))
        (restore-context uc context)
        (is (= (read-register uc :eax) 2))
        (start uc :begin address :until (+ address 1))
        (is (= (read-register uc :eax) 3))
        (restore-context uc context)
        (is (= (read-register uc :eax) 2))))))
