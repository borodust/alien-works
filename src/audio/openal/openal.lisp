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
           (%al:distance-model %al:+inverse-distance-clamped+)
           (unwind-protect
                (progn ,@body)
             (%alc:destroy-context ,ctx)
             (%alc:close-device ,dev)))))))


(defun extension-supported-p (name &optional device)
  (/= 0 (%alc:is-extension-present device name)))


;;;
;;; AUDIO DEVICE
;;;
(defun all-output-audio-devices ()
  (if (extension-supported-p "ALC_enumeration_EXT")
      (cref:c-let ((str :char :from (%alc:get-string nil %alc:+all-devices-specifier+)))
        (loop with start = 0
              for i from 0
              for char = (str i)
              until (and (= char 0) (= start i))
              when (= char 0)
                collect (prog1 (cffi:foreign-string-to-lisp (str start &) :count (- i start))
                          (setf start (1+ i)))))
      (list (%alc:get-string nil %alc:+device-specifier+))))


(defmacro do-output-audio-devices ((device-name) &body body)
  `(loop for ,device-name in (all-output-audio-devices)
         do (progn ,@body)))

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

          (math:vec3 up-result-vec3 0) (orientation 3)
          (math:vec3 up-result-vec3 1) (orientation 4)
          (math:vec3 up-result-vec3 2) (orientation 5)))
  (values at-result-vec3 up-result-vec3))


(defun (setf audio-listener-orientation) (at-result-vec3 up-result-vec3)
  (cref:c-with ((orientation %al:float :count 6))
    (setf (orientation 0) (math:vec3 at-result-vec3 0)
          (orientation 1) (math:vec3 at-result-vec3 1)
          (orientation 2) (math:vec3 at-result-vec3 2)

          (orientation 3)  (math:vec3 up-result-vec3 0)
          (orientation 4)  (math:vec3 up-result-vec3 1)
          (orientation 5) (math:vec3 up-result-vec3 2))
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


(defun (setf audio-buffer-data) (s16-mono-48k-pcm-data buffer)
  (sv:with-static-vector (foreign-data (length s16-mono-48k-pcm-data)
                                       :element-type '(signed-byte 16)
                                       :initial-contents s16-mono-48k-pcm-data)
    ;; Load sample data into the buffer
    (%al:buffer-data buffer %al:+format-mono16+
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
    (%al:delete-sources 1 (src &)))
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


(defun pause-audio-source (source)
  (%al:source-pause source))


(defun stop-audio-source (source)
  (%al:source-stop source))


(defun audio-source-pitch (source)
  (cref:c-with ((val %al:float))
    (%al:get-sourcef source %al:+pitch+ (val &))
    val))


(defun (setf audio-source-pitch) (value source)
  (%al:sourcef source %al:+pitch+ (float (max 0 value) 0f0))
  value)


(defun audio-source-looping-p (source)
  (cref:c-with ((val %al:int))
    (%al:get-sourcei source %al:+looping+ (val &))
    (/= val %al:+false+)))


(defun (setf audio-source-looping-p) (value source)
  (%al:sourcei source %al:+looping+ (if value %al:+true+ %al:+false+))
  value)


(defun audio-source-gain (source)
  (cref:c-with ((val %al:float))
    (%al:get-sourcef source %al:+gain+ (val &))
    val))


(defun (setf audio-source-gain) (value source)
  (%al:sourcef source %al:+gain+ (float (max 0 value) 0f0))
  value)


(defun audio-source-reference-distance (source)
  (cref:c-with ((val %al:float))
    (%al:get-sourcef source %al:+reference-distance+ (val &))
    val))


(defun (setf audio-source-reference-distance) (value source)
  (%al:sourcef source %al:+reference-distance+ (float (max 0 value) 0f0))
  value)


(defun audio-source-max-distance (source)
  (cref:c-with ((val %al:float))
    (%al:get-sourcef source %al:+max-distance+ (val &))
    val))


(defun (setf audio-source-max-distance) (value source)
  (%al:sourcef source %al:+max-distance+ (float (max 0 value) 0f0))
  value)


(defun audio-source-rolloff (source)
  (cref:c-with ((value %al:float))
    (%al:get-sourcef source %al:+rolloff-factor+ (value &))
    value))


(defun (setf audio-source-rolloff) (value source)
  (%al:sourcef source %al:+rolloff-factor+ (float (max 0 value) 0f0))
  value)


(defun audio-source-position (source result-vec3)
  (%al:get-sourcefv source %al:+position+ (%math:vec3-ptr result-vec3))
  result-vec3)


(defun (setf audio-source-position) (vec3 source)
  (%al:sourcefv source %al:+position+ (%math:vec3-ptr vec3))
  vec3)


(defun audio-source-velocity (source result-vec3)
  (%al:get-sourcefv source %al:+velocity+ (%math:vec3-ptr result-vec3))
  result-vec3)


(defun (setf audio-source-velocity) (vec3 source)
  (%al:sourcefv source %al:+velocity+ (%math:vec3-ptr vec3))
  vec3)


(defun audio-source-direction (source result-vec3)
  (%al:get-sourcefv source %al:+direction+ (%math:vec3-ptr result-vec3))
  result-vec3)


(defun (setf audio-source-direction) (vec3 source)
  (%al:sourcefv source %al:+direction+ (%math:vec3-ptr vec3))
  vec3)


(defun audio-source-offset (source &optional (type :seconds))
  (if (eq type :seconds)
      (cref:c-with ((value %al:float))
        (%al:get-sourcef source %al:+sec-offset+ (value &))
        value)
      (cref:c-with ((value %al:uint))
        (%al:get-sourcei source
                         (ecase type
                           (:samples %al:+sample-offset+)
                           (:bytes %al:+byte-offset+))
                         (value &))
        value)))


;;;
;;; MISC
;;;
(defun wait-for-source (source)
  "Wait until provided source is not in AL_PLAYING state"
  (loop while (eq (audio-source-state source) :playing)
        do (sleep 0.1)))


(defun play-pcm-s16-mono (pcm-data)
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
