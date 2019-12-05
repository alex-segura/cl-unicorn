;;;; autowrap.lisp

(cl:in-package #:unicorn-ffi)

(autowrap:c-include
 "/usr/local/include/unicorn/unicorn.h"
 :accessor-package #:unicorn-ffi.accessors
 :function-package #:unicorn-ffi.functions
 :exclude-sources ("/usr/include")
 :include-sources ("stdint.h"
                   "bits/types.h"
                   "sys/types.h"
                   "sys/_types.h"
                   "bits/stdint"
                   "machine/_types.h")
 :exclude-definitions ("truncate" "ftruncate" "abs")
 :spec-path '(:cl-unicorn :autowrap-spec))
