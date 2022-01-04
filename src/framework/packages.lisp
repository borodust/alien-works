(cl:defpackage :alien-works.framework
  (:local-nicknames (:a :alexandria)
                    (:cref :cffi-c-ref)
                    (:sv :static-vectors)

                    (:u :alien-works.utils)
                    (:math :alien-works.math)
                    (:host :alien-works.host)
                    (:audio :alien-works.audio)
                    (:graphics :alien-works.graphics)
                    (:physics :alien-works.physics)

                    (:%host :%alien-works.host)
                    (:%audio :%alien-works.audio)
                    (:%graphics :%alien-works.graphics))
  (:use :cl)
  (:export #:with-alien-works))
