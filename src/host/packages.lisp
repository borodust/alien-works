(cl:defpackage :alien-works.host
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria))
  (:use :cl)
  (:export #:with-window
           #:window-surface))
