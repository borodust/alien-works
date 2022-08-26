(cl:defpackage :%alien-works.tools.filament
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty)
                    (:u :alien-works.utils)
                    (:m :alien-works.math))
  (:use :cl)
  (:export #:serialize-material
           #:material-data-pointer
           #:material-data-size
           #:with-serialized-material-data))


(cl:defpackage :alien-works.tools.graphics
  (:local-nicknames (:a :alexandria)
                    (:%aw.fm :%alien-works.filament)
                    (:%gx :%alien-works.graphics)
                    (:gx :alien-works.graphics)
                    (:%gxs :%alien-works.tools.filament)
                    (:u :alien-works.utils)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref)
                    (:m :alien-works.math)
                    (:mem :alien-works.memory)
                    (:%mem :%alien-works.memory)
                    (:host :alien-works.host))
  (:use :cl)
  (:export #:make-material
           #:serialize-material))
