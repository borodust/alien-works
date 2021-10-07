(cl:in-package :alien-works.audio.openal)


(defmacro with-context (() &body body)
  (alexandria:with-gensyms (dev ctx)
    `(float-features:with-float-traps-masked ()
       ;; Open default sound device
       (let ((,dev (%alc:open-device nil)))
         (when (cffi:null-pointer-p ,dev)
           (error "Couldn't open sound device"))
         ;; Create OpenAL context for opened device
         (let ((,ctx (%alc:create-context ,dev nil)))
           (when (cffi:null-pointer-p ,ctx)
             (error "Failed to create OpenAL context"))
           ;; Assign OpenAL context to the application
           (%alc:make-context-current ,ctx)
           (unwind-protect
                (progn ,@body)
             (%alc:destroy-context ,ctx)
             (%alc:close-device ,dev)))))))

;;;
;;; LISTENER
;;;
(defun audio-listener-gain ()
  (cref:c-with ((value :float))
    (%al:get-listenerf %al:+gain+ (value &))
    value))


(defun (setf audio-listener-gain) (value)
  (%al:listenerf %al:+gain+ (float value 0f0))
  value)


(defun audio-listener-position (result-vec3)
  (%al:get-listenerfv %al:+position+ (%math:vec3-ptr result-vec3))
  result-vec3)


(defun (setf audio-listener-position) (value-vec3)
  (%al:listenerfv %al:+position+ (%math:vec3-ptr value-vec3))
  value-vec3)


(defun audio-listener-velocity (result-vec3)
  (%al:get-listenerfv %al:+velocity+ (%math:vec3-ptr result-vec3))
  result-vec3)


(defun (setf audio-listener-velocity) (value-vec3)
  (%al:listenerfv %al:+velocity+ (%math:vec3-ptr value-vec3))
  value-vec3)


(defun audio-listener-orientation (at-result-vec3 up-result-vec3)
  (cref:c-with ((orientation %al:float :count 6))
    (%al:get-listenerfv %al:+orientation+ (orientation &))
    (setf (math:vec3 at-result-vec3 0) (orientation 0)
          (math:vec3 at-result-vec3 1) (orientation 1)
          (math:vec3 at-result-vec3 2) (orientation 2)

          (math:vec3 up-result-vec3 0) (orientation 0)
          (math:vec3 up-result-vec3 1) (orientation 1)
          (math:vec3 up-result-vec3 2) (orientation 2)))
  (values at-result-vec3 up-result-vec3))


(defun (setf audio-listener-orientation) (at-result-vec3 up-result-vec3)
  (cref:c-with ((orientation %al:float :count 6))
    (setf (orientation 0) (math:vec3 at-result-vec3 0)
          (orientation 1) (math:vec3 at-result-vec3 1)
          (orientation 2) (math:vec3 at-result-vec3 2)

          (orientation 0)  (math:vec3 up-result-vec3 0)
          (orientation 1)  (math:vec3 up-result-vec3 1)
          (orientation 2) (math:vec3 up-result-vec3 2))
    (%al:listenerfv %al:+orientation+ (orientation &)))
  (values at-result-vec3 up-result-vec3))


;;;
;;; BUFFER
;;;
(defun make-audio-buffer ()
  (cref:c-with ((buf %al:uint))
    (setf buf 0)
    (%al:gen-buffers 1 (buf &))
    buf))


(defun (setf audio-buffer-data) (s16-stereo-48k-pcm-data buffer)
  (sv:with-static-vector (foreign-data (length s16-stereo-48k-pcm-data)
                                       :element-type '(signed-byte 16)
                                       :initial-contents s16-stereo-48k-pcm-data)
    ;; Load sample data into the buffer
    (%al:buffer-data buffer %al:+format-stereo16+
                     (static-vectors:static-vector-pointer foreign-data)
                     (* (length foreign-data) 2)
                     48000)))


(defun destroy-audio-buffer (buffer)
  (cref:c-with ((buf %al:uint))
    (setf buf buffer)
    (%al:delete-buffers 1 (buf &)))
  (values))


;;;
;;; SOURCE
;;;
(defun make-audio-source ()
  (cref:c-with ((source %al:uint))
    (setf source 0)
    (%al:gen-sources 1 (source &))
    source))


(defun destroy-audio-source (source)
  (cref:c-with ((src %al:uint))
    (setf src source)
    (%al:gen-sources 1 (src &)))
  (values))


(defun audio-source-state (source)
  (cref:c-with ((state %al:int))
    (%al:get-sourcei source %al:+source-state+ (state &))
    (case state
      (#.%al:+initial+ :initial)
      (#.%al:+playing+ :playing)
      (#.%al:+paused+ :paused)
      (#.%al:+stopped+ :stopped))))


(defun audio-source-buffer (source)
  (cref:c-with ((buf %al:uint))
    (%al:get-sourcei source %al:+buffer+ (buf &))
    buf))


(defun (setf audio-source-buffer) (buffer source)
  (%al:sourcei source %al:+buffer+ buffer)
  buffer)


(defun play-audio-source (source)
  (%al:source-play source))


;;;
;;; MISC
;;;
(defun wait-for-source (source)
  "Wait until provided source is not in AL_PLAYING state"
  (loop while (eq (audio-source-state source) :playing)
        do (sleep 0.1)))


(defun play-pcm-s16-stereo (pcm-data)
  (let ((buffer (make-audio-buffer))
        (source (make-audio-source)))
    (unwind-protect
         (progn
           (setf (audio-buffer-data buffer) pcm-data
                 (audio-source-buffer source) buffer)
           (play-audio-source source)
           (wait-for-source source))
      (destroy-audio-buffer buffer)
      (destroy-audio-source source))))
