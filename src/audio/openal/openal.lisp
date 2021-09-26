(cl:in-package :alien-works.audio.openal)


(defun wait-for-source (source)
  "Wait until provided source is not in AL_PLAYING state"
  (cref:c-with ((playing-p %al:int))
    (setf playing-p %al:+playing+)
    (loop while (= playing-p %al:+playing+) do
      (%al:get-sourcei source %al:+source-state+ (playing-p &))
      (sleep 0.2))))


(defun play-pcm-s16-stereo (pcm-data)
  (cref:c-with ((buf %al:uint)
                (source %al:uint))
    (setf buf 0
          source 0)
    (unwind-protect
         (progn
           ;; Generate buffer to hold our sample data
           (%al:gen-buffers 1 (buf &))
           (static-vectors:with-static-vector (foreign-data (length pcm-data)
                                                            :element-type '(signed-byte 16)
                                                            :initial-contents pcm-data)
             ;; Load sample data into the buffer
             (%al:buffer-data buf %al:+format-stereo16+
                              (static-vectors:static-vector-pointer foreign-data)
                              (* (length foreign-data) 2)
                              48000))
           ;; Generate a source to play the sample
           (%al:gen-sources 1 (source &))
           ;; Attach our buffer to the source
           (%al:sourcei source %al:+buffer+ buf)
           ;; Play a sound
           (%al:source-play source)
           (wait-for-source source))
      ;; Delete the buffer and the source
      (unless (zerop source)
        (%al:delete-sources 1 (source &)))
      (unless (zerop buf)
        (%al:delete-buffers 1 (buf &))))))


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
