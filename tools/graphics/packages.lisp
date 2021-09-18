(cl:defpackage :%alien-works.tools.filament
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


(cl:defpackage :alien-works.tools.graphics
  (:local-nicknames (:a :alexandria)
                    (:%gx :%alien-works.filament)
                    (:gx :alien-works.graphics)
                    (:%gxs :%alien-works.tools.filament)
                    (:u :alien-works.utils)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref)
                    (:m :alien-works.math))
  (:use :cl)
  (:export #:make-material
           #:parse-material
           #:destroy-material))
