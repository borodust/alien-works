(cl:defpackage :%alien-works.memory
  (:use)
  (:export #:memory-vector-pointer))


(cl:defpackage :alien-works.memory
  (:local-nicknames (:a :alexandria)
                    (:u :alien-works.utils)
                    (:cltl2 :alien-works.cltl2)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref))
  (:use :cl :%alien-works.memory)
  (:export #:with-system-memory-allocator

           #:make-stack-allocator
           #:destroy-stack-allocator
           #:with-stack-allocator
           #:stack-alloc
           #:stack-free

           #:define-memory-layout
           #:memory-layout-size
           #:memory-layout-slot-offset
           #:access-memory
           #:with-memory-access

           #:make-memory-vector
           #:destroy-memory-vector
           #:with-memory-vector
           #:with-memory-vectors))
