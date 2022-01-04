(cl:in-package :alien-works.audio)


(defun decode-audio (octet-stream-in &key (channels 1))
  (flexi-streams:with-output-to-sequence (out :element-type '(signed-byte 16))
    (%aw.opus:decode-audio octet-stream-in out 48000 channels)))


(defun encode-audio (s16-mono-pcm-stream-in octet-stream-out &key (channels 1))
  (let ((frame-duration 20) ;; msec
        (sample-rate 48000))
    (%aw.opus:encode-audio s16-mono-pcm-stream-in octet-stream-out
                           (* (/ sample-rate 1000) frame-duration channels)
                           sample-rate
                           channels)))


(defun play-audio (s16-mono-pcm)
  (%aw.al:play-pcm-s16-mono s16-mono-pcm))


(defmacro %audio:with-audio (() &body body)
  `(%aw.al:with-context ()
     ,@body))


(defun make-audio-buffer (s16-48k-pcm &key (channels 1))
  (let ((buffer (%aw.al:make-audio-buffer)))
    (setf (%aw.al:audio-buffer-data buffer :channels channels) s16-48k-pcm)
    buffer))


(defun destroy-audio-buffer (buffer)
  (%aw.al:destroy-audio-buffer buffer))


(defun make-audio-source (buffer)
  (let ((source (%aw.al:make-audio-source)))
    (setf (%aw.al:audio-source-buffer source) buffer)
    source))


(defun make-audio-source-from-pcm (s16-48k-pcm &key (channels 1))
  (let ((buffer (%aw.al:make-audio-buffer))
        (source (%aw.al:make-audio-source)))
    (setf (%aw.al:audio-buffer-data buffer :channels channels) s16-48k-pcm
          (%aw.al:audio-source-buffer source) buffer)
    (values source buffer)))


(defun destroy-audio-source (source)
  (%aw.al:destroy-audio-source source))


(defun play-audio-source (source)
  (%aw.al:play-audio-source source))


(defun pause-audio-source (source)
  (%aw.al:pause-audio-source source))


(defun stop-audio-source (source)
  (%aw.al:stop-audio-source source))


(defun audio-source-state (source)
  (%aw.al:audio-source-state source))
