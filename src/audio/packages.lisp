(cl:defpackage :alien-works.audio.openal
  (:local-nicknames (:u :alien-works.utils)
                    (:cref :cffi-c-ref)
                    (:%math :%alien-works.math)
                    (:math :alien-works.math)
                    (:sv :static-vectors))
  (:use :cl)
  (:export #:with-context
           #:do-output-audio-devices

           #:play-pcm-s16-mono

           #:audio-listener-gain
           #:audio-listener-position
           #:audio-listener-velocity
           #:audio-listener-orientation

           #:make-audio-buffer
           #:audio-buffer-data
           #:destroy-audio-buffer

           #:make-audio-source
           #:destroy-audio-source
           #:audio-source-state
           #:audio-source-buffer
           #:play-audio-source
           #:pause-audio-source
           #:stop-audio-source
           #:audio-source-pitch
           #:audio-source-gain
           #:audio-source-distance
           #:audio-source-max-distance
           #:audio-source-reference-distance
           #:audio-source-rolloff
           #:audio-source-position
           #:audio-source-velocity
           #:audio-source-direction
           #:audio-source-offset
           #:audio-source-looping-p))


(cl:defpackage :alien-works.audio.opus
  (:local-nicknames (:u :alien-works.utils)
                    (:host :alien-works.host)
                    (:a :alexandria)
                    (:cref :cffi-c-ref)
                    (:sv :static-vectors))
  (:use :cl)
  (:export #:encode-audio
           #:decode-audio))


(cl:defpackage :%alien-works.audio
  (:export #:with-audio))


(cl:defpackage :alien-works.audio
  (:local-nicknames (:%aw.al :alien-works.audio.openal)
                    (:%aw.opus :alien-works.audio.opus)
                    (:%audio :%alien-works.audio))
  (:use :cl)
  (:import-from :alien-works.audio.openal
                #:do-output-audio-devices
                #:audio-listener-gain
                #:audio-listener-position
                #:audio-listener-velocity
                #:audio-listener-orientation
                #:audio-source-pitch
                #:audio-source-gain
                #:audio-source-distance
                #:audio-source-max-distance
                #:audio-source-reference-distance
                #:audio-source-rolloff
                #:audio-source-position
                #:audio-source-velocity
                #:audio-source-direction
                #:audio-source-offset
                #:audio-source-looping-p)
  (:export #:play-audio
           #:decode-audio
           #:encode-audio

           #:make-audio-buffer
           #:destroy-audio-buffer

           #:make-audio-source
           #:make-audio-source-from-pcm
           #:destroy-audio-source
           #:play-audio-source
           #:pause-audio-source
           #:stop-audio-source
           #:audio-source-state
           #:audio-source-pitch
           #:audio-source-gain
           #:audio-source-distance
           #:audio-source-max-distance
           #:audio-source-reference-distance
           #:audio-source-rolloff
           #:audio-source-position
           #:audio-source-velocity
           #:audio-source-direction
           #:audio-source-offset
           #:audio-source-looping-p

           #:do-output-audio-devices
           #:audio-listener-gain
           #:audio-listener-position
           #:audio-listener-velocity
           #:audio-listener-orientation))
