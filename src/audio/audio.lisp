(cl:in-package :alien-works.audio)


(defun decode-audio (octet-stream-in)
  (flexi-streams:with-output-to-sequence (out :element-type '(signed-byte 16))
    (%aw.opus:decode-audio octet-stream-in out 48000 2)))


(defun encode-audio (s16-stereo-pcm-stream-in octet-stream-out)
  (let ((frame-duration 20) ;; msec
        (sample-rate 48000)
        (channels 2))
    (%aw.opus:encode-audio s16-stereo-pcm-stream-in octet-stream-out
                           (* (/ sample-rate 1000) frame-duration channels)
                           sample-rate
                           channels)))


(defun play-audio (s16-stereo-pcm)
  (%aw.al:play-pcm-s16-stereo s16-stereo-pcm))


(defmacro with-audio (() &body body)
  `(%aw.al:with-context ()
     ,@body))
