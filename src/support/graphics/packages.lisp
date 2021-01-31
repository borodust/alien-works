(cl:defpackage :%alien-works.support.graphics
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty)
                    (:u :alien-works.utils)
                    (:m :alien-works.math))
  (:use :cl)
  (:export #:parse-material
           #:destroy-material
           #:material-data
           #:material-size
           #:with-parsed-material))


(cl:defpackage :alien-works.support.graphics
  (:local-nicknames (:a :alexandria)
                    (:%gx :%alien-works.graphics)
                    (:gx :alien-works.graphics)
                    (:%gxs :%alien-works.support.graphics)
                    (:u :alien-works.utils)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref)
                    (:m :alien-works.math))
  (:use :cl)
  (:export #:make-material
           #:parse-material
           #:destroy-material))
