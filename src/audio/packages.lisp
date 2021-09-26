(cl:defpackage :alien-works.audio.openal
  (:local-nicknames (:u :alien-works.utils)
                    (:cref :cffi-c-ref))
  (:use :cl)
  (:export #:with-context
           #:play-pcm-s16-stereo))


(cl:defpackage :alien-works.audio.opus
  (:local-nicknames (:u :alien-works.utils)
                    (:host :alien-works.host)
                    (:a :alexandria)
                    (:cref :cffi-c-ref)
                    (:sv :static-vectors))
  (:use :cl)
  (:export #:encode-audio
           #:decode-audio))


(cl:defpackage :alien-works.audio
  (:local-nicknames (:%aw.al :alien-works.audio.openal)
                    (:%aw.opus :alien-works.audio.opus))
  (:use :cl)
  (:export #:with-audio
           #:play-audio
           #:decode-audio
           #:encode-audio))
