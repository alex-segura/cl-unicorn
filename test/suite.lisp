;;;; suite.lisp

(in-package #:unicorn-test)

(def-suite unicorn
  :description "Top-level suite for CL-UNICORN")

(def-suite x86
  :description "Tests for X86"
  :in unicorn)
