;;;; util.lisp

(in-package #:unicorn-test)

(defun make-bytes (&rest bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))
