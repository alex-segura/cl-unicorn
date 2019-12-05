;;;; cl-unicorn.asd

(defsystem cl-unicorn
  :name "cl-unicorn"
  :description "Bindings to the unicorn engine emulator library"
  :version "0.0.1"
  :author "Alex Segura <alex@lispm.dev>"
  :depends-on (#:alexandria
               #:cl-autowrap/libffi
               #:cl-plus-c)
  :in-order-to ((test-op (test-op #:cl-unicorn-test)))
  :pathname "src"
  :serial t
  :components
  ((:module :autowrap-spec
    :pathname "spec"
    :components ((:static-file "unicorn.arm-pc-linux-gnu.spec")
                 (:static-file "unicorn.i386-unknown-freebsd.spec")
                 (:static-file "unicorn.i386-unknown-openbsd.spec")
                 (:static-file "unicorn.i686-apple-darwin9.spec")
                 (:static-file "unicorn.i686-pc-linux-gnu.spec")
                 (:static-file "unicorn.i686-pc-windows-msvc.spec")
                 (:static-file "unicorn.x86_64-apple-darwin9.spec")
                 (:static-file "unicorn.x86_64-pc-linux-gnu.spec")
                 (:static-file "unicorn.x86_64-pc-windows-msvc.spec")
                 (:static-file "unicorn.x86_64-unknown-freebsd.spec")
                 (:static-file "unicorn.x86_64-unknown-openbsd.spec")))
   (:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "unicorn")))

(defsystem cl-unicorn-test
  :name "cl-unicorn-test"
  :description "Test suite for CL-UNICORN library"
  :version "0.0.1"
  :author "Alex Segura <alex@lispm.dev>"
  :depends-on (#:cl-unicorn #:fiveam)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam
                                      '#:run!
                                      (uiop:find-symbol* '#:unicorn :unicorn-test)))
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:file "suite")
   (:file "util")))
