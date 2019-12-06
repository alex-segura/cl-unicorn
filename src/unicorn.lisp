;;;; unicorn.lisp

(in-package #:unicorn)

(define-condition unicorn-error (error)
  ((code :initform 0
         :initarg :code
         :accessor unicorn-error-code))
  (:report (lambda (condition stream)
             (format stream "~a"
                     (uc-strerror (unicorn-error-code condition))))))

(defmacro check-rc (form)
  (alexandria:once-only (form)
    (alexandria:with-gensyms (rc)
      `(let ((,rc ,form))
         (unless (= ,rc 0)
           (error 'unicorn-error :code ,rc))))))

(defun version ()
  (c-with ((major :int)
           (minor :int))
    (uc-version (major &) (minor &))
    (values major minor)))

(defvar *architecture*)
(defvar *hooks* nil)

(defun register-enum-for-architecture (architecture)
  (case architecture
    (:arm 'unicorn-ffi:uc-arm-reg)
    (:arm64 'unicorn-ffi:uc-arm64-reg)
    (:mips 'unicorn-ffi:uc-mips-reg)
    (:x86 'unicorn-ffi:uc-x86-reg)
    (:sparc 'unicorn-ffi:uc-sparc-reg)
    (:m68k 'unicorn-ffi:uc-m68k-reg)
    (t (error 'unicorn-error :code unicorn-ffi:+UC-ERR-ARCH+))))

(defun register-enum-value (reg)
  (enum-value (register-enum-for-architecture *architecture*) reg))

(defun architecture-supported-p (arch)
  (when (enum-value 'unicorn-ffi:uc-arch arch)
    (not (zerop (uc-arch-supported arch)))))

(defun list-supported-architectures ()
  (let ((vals (autowrap:foreign-enum-values (autowrap:find-type 'unicorn-ffi:uc-arch))))
    (mapcar #'car
            (remove-if-not #'architecture-supported-p vals :key #'car))))

(defun open-engine (architecture mode)
  (c-with ((uc* :pointer))
    (check-rc (uc-open architecture mode (uc* &)))
    (autowrap:wrap-pointer uc* 'unicorn-ffi:uc-engine)))

(defun close-engine (engine)
  (check-rc (uc-close engine))
  (invalidate engine))

(defun query (engine query)
  (c-with ((r :unsigned-int))
    (check-rc (uc-query engine query (r &)))
    r))

(defun write-register (engine register value)
  "Write VALUE to REGISTER of ENGINE."
  (if (listp value)
      (c-with ((val :unsigned-long-long :count (length value)))
        (loop :for i :upfrom 0
              :for elt :in value
              :do (setf (val i) elt))
        (check-rc (uc-reg-write engine (register-enum-value register) (val &))))
      (c-with ((val :unsigned-long-long))
        (setf val value)
        (check-rc (uc-reg-write engine (register-enum-value register) (val &))))))

(defun read-register (engine register &key (size 1))
  "Read the REGISTER of ENGINE."
  (assert (> size 0) (size)
          "Size must be greater than zero: ~s" size)
  (c-with ((val :unsigned-long-long :count size))
    (setf val 0)
    (check-rc (uc-reg-read engine (register-enum-value register) (val &)))
    (if (= size 1)
        val
        (loop :repeat size
              :for i :upfrom 0
              :collect (val i)))))

(defun write-memory (engine address bytes)
  "Write BYTES to the memory of ENGINE, starting at ADDRESS."
  (cffi:with-foreign-array (vals bytes `(:array :uint8 ,(length bytes)))
    (check-rc (uc-mem-write engine address vals (length bytes)))))

(defun read-memory (engine address count)
  "Read COUNT bytes from ENGINE, starting at ADDRESS."
  (cffi:with-foreign-pointer (bytes count)
    (check-rc (uc-mem-read engine address bytes count))
    (cffi:foreign-array-to-lisp bytes `(:array :uint8 ,count))))

(defun start (engine &key (begin 0) (until 0) (timeout 0) (count 0))
  "Start the emulation of ENGINE. The default behavior is to run until completion (or error).
The keyword arguments alter this:
- :BEGIN is the (optional) start address.
- :UNTIL (if greater than BEGIN) is the (optional) end address.
- :TIMEOUT (if nonzero) is the time (in usecs) to emulate.
- :COUNT (if nonzero) is the number of instructions to emulate."
  (check-rc (uc-emu-start engine begin until timeout count)))

(defconstant +second-scale+ unicorn-ffi:+uc-second-scale+)
(defconstant +millisecond-scale+ unicorn-ffi:+uc-milisecond-scale+)

(defun stop (engine)
  "Stop the emulation of ENGINE."
  (check-rc (uc-emu-stop engine)))

(autowrap:define-bitmask-from-constants (uc-prot)
  unicorn-ffi:+uc-prot-all+
  unicorn-ffi:+uc-prot-exec+
  unicorn-ffi:+uc-prot-none+
  unicorn-ffi:+uc-prot-read+
  unicorn-ffi:+uc-prot-write+)

#+nil
(defcallback memory-callback :void
    ((uc :pointer)
     (address :unsigned-long-long)
     (size :unsigned-int)
     (user-data :pointer))
  (let ((gethash (autowrap:) *hooks*))))

(defun add-hook (engine type function
                 &key (user-data (cffi:null-pointer))
                   (begin 1) (end 0) (instruction-id nil))
  "Add a hook to ENGINE. TYPE must be a valid HOOK-TYPE and FUNCTION must designate a C
callback. USER-DATA is an optional pointer to .
BEGIN and END specify the valid range for the callback function."
  (c-with ((hook unicorn-ffi:uc-hook)
           (user-data :pointer))
    (check-rc
     (uc-hook-add engine
                  (hook &)
                  (enum-value 'unicorn-ffi:uc-hook-type type)
                  (callback function)
                  user-data
                  begin
                  end
                  :unsigned-long-long
                  (if instruction-id
                      (enum-value 'unicorn-ffi:uc-x86-insn instruction-id)
                      0)))
    hook))

(defun remove-hook (engine hook)
  "Remove HOOK from ENGINE."
  (check-rc (uc-hook-del engine hook)))

(defun map-memory (engine address size &rest permissions)
  "Map a region of memory in ENGINE."
  (check-rc (uc-mem-map engine address size (mask-apply 'uc-prot permissions))))

(defun unmap-memory (engine address size)
  "Unmap a region of memory from ENGINE."
  (check-rc (uc-mem-unmap engine address size)))

(defun set-memory-permissions (engine address size &rest permissions)
  "Adjust the memory protection within ENGINE to PERMISSIONS for the address range ADDRESS to
ADDRESS + SIZE."
  (check-rc (uc-mem-protect engine address size (mask-apply 'uc-prot permissions))))

#+nil
(progn
(defun list-memory-regions (engine)
  (c-with ((regions :pointer)
           (count :unsigned-int))
    (check-rc (uc-mem-regions engine (regions &) (count &)))
    (loop :repeat count
          :for i :upfrom 0
          :collect (let* ((ptr (autowrap:c-aref regions i))
                          (region (autowrap:wrap-pointer ptr 'unicorn-ffi:uc-mem-region)))
                     (tg:finalize region (lambda () (uc-free ptr)))))))

(defun memory-region-begin (region)
  (uc-mem-region.begin region))

(defun memory-region-end (region)
  (uc-mem-region.end region))

(defun memory-region-permissions (region)
  (mask-keywords 'uc-prot (uc-mem-region.perms region)))
)

(defmacro with-open-engine ((var architecture mode) &body body)
  "Bind VAR to an open unicorn engine using ARCHITECTURE and MODE within the dynamic context of
BODY."
  `(let ((,var (open-engine ,architecture ,mode))
         (*architecture* ,architecture))
     (unwind-protect (progn ,@body)
       (close-engine ,var))))

(defun make-context (engine)
  (c-with ((context* :pointer))
    (check-rc (uc-context-alloc engine (context* &)))
    (let* ((context (autowrap:wrap-pointer context* 'unicorn-ffi:uc-context))
           (ptr (ptr context)))
      (tg:finalize context (lambda () (uc-free ptr)))
      context)))

(defun save-context (engine)
  (let ((context (make-context engine)))
    (check-rc (uc-context-save engine context))
    context))

(defun restore-context (engine context)
  (check-rc (uc-context-restore engine context)))
