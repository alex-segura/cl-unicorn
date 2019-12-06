;;;; package.lisp

(pushnew :unicorn *features*)

(defpackage #:unicorn-ffi)
(defpackage #:unicorn-ffi.accessors)
(defpackage #:unicorn-ffi.functions)

(defpackage #:unicorn
  (:use #:cl
        #:autowrap.minimal
        #:plus-c
        #:unicorn-ffi.accessors
        #:unicorn-ffi.functions)
  (:nicknames #:uc)
  (:export #:+second-scale+
           #:+millisecond-scale+
           #:unicorn-error
           #:open-engine
           #:close-engine
           #:with-open-engine
           #:write-register
           #:read-register
           #:write-memory
           #:read-memory
           #:start
           #:stop
           #:add-hook
           #:remove-hook
           #:map-memory
           #:unmap-memory
           #:set-memory-permissions
           #:make-context
           #:save-context
           #:restore-context))
