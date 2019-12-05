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
  (:export #:with-emulator
           #:open-engine
           #:close-engine
           #:write-register
           ;; #:write-registers
           #:read-register
           ;; #:read-registers
           #:write-memory
           #:read-memory
           #:start-emulator
           #:stop-emulator
           #:add-hook
           #:remove-hook
           #:map-memory
           #:unmap-memory
           #:set-memory-permissions
           #:list-memory-regions))
