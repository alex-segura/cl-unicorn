;;;; library.lisp

(in-package #:unicorn)

(cffi:define-foreign-library unicorn
  (:unix "libunicorn.so.1")
  (t (:default "libunicorn.so")))

(defun load-unicorn-library ()
  (cffi:use-foreign-library unicorn))

(unless (cffi:foreign-library-loaded-p 'unicorn)
  (load-unicorn-library))
