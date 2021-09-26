(cl:in-package :alien-works.audio.opus)


(declaim (special *length-buffer*))

(a:define-constant +recommended-packet-byte-size+ 4095)

(a:define-constant +max-samples-per-channel+ (* (/ 48000 ;; 48KHz sample rate
                                                   1000) ;; per ms
                                                60))     ;; 60ms frame duration


(defun decode-packet-length (stream)
  (a:when-let (byte (read-byte stream nil nil))
    (if (< byte #b10000000)
        byte
        (let ((next-bytes (1+ (logand (ash byte -4) #b0111))))
          (read-sequence *length-buffer* stream :end next-bytes)
          (loop with length = (logand byte #b00001111)
                for i from 0 below next-bytes
                for byte = (aref *length-buffer* i)
                do (setf length (logior (ash length 8) byte))
                finally (return length))))))


(defun encode-packet-length (stream length)
  (if (< length #b10000000)
      (write-byte length stream)
      (flet ((write-len-bytes (len-bytes value)
               (loop for i from (1- len-bytes) downto 0
                     do (write-byte (ldb (byte 8 (* i 8)) value) stream)))
             (write-len-size (len-byte-size &optional (tail-value 0))
               (write-byte (logior #b10000000
                                   (ash (1- len-byte-size) 4)
                                   tail-value)
                           stream)))
        (let* ((bit-len (integer-length length))
               (tail-bits (mod bit-len 8))
               (len-bytes (floor (/ bit-len 8))))
          (when (> bit-len 68)
            (error "Cannot encode length ~A: max integer length is 68 bits" length))
          (cond
            ((zerop tail-bits)
             (write-len-size len-bytes)
             (write-len-bytes len-bytes length))
            ((<= tail-bits 4)
             (let ((tail-value (ldb (byte 8 (* 8 len-bytes)) length)))
               (write-len-size len-bytes tail-value)
               (write-len-bytes len-bytes (logxor length
                                                  (ash tail-value (* 8 len-bytes))
                                                  (ash #b00001111 (* 8 len-bytes))))))
            (t (write-len-size (1+ len-bytes))
               (write-len-bytes (1+ len-bytes) length)))))))


(defun encode-packet (packet-stream packet &key start end)
  (let ((end (or end (length packet)))
        (start (or start 0)))
    (encode-packet-length packet-stream (- end start))
    (write-sequence packet packet-stream :start start :end end)
    (values)))


(define-condition packet-overflow (serious-condition)
  ((required-size :initarg :size :reader packet-required-size)))


(defun try-different-array (new-array &key ((:start new-start) nil start-provided-p)
                                        ((:end new-end) nil end-provided-p))
  (a:if-let (restart (find-restart 'try-different-array))
    (apply #'invoke-restart
           restart
           new-array
           (nconc (when start-provided-p
                    (list :start new-start))
                  (when end-provided-p
                    (list :end new-end))))
    (warn "~A restart not found" 'try-different-array)))


(defun decode-packet (packet-stream packet &key start end)
  (a:if-let (packet-len (decode-packet-length packet-stream))
    (tagbody retry
       (restart-case
           (let ((end (or end (length packet)))
                 (start (or start 0)))
             (when (< (- end start) packet-len)
               (error (make-condition 'packet-overflow :size packet-len)))
             (let* ((bytes-read (read-sequence packet
                                               packet-stream
                                               :start start
                                               :end (+ start packet-len))))
               (unless (= bytes-read packet-len)
                 (error "Malformed packet: expected size ~A, but got ~A"
                        packet-len bytes-read))
               (return-from decode-packet (values packet bytes-read))))
         (try-different-array (new-array &key ((:start new-start) nil start-provided-p)
                                         ((:end new-end) nil end-provided-p))
           (when new-array
             (setf packet new-array))
           (when start-provided-p
             (setf start new-start))
           (when end-provided-p
             (setf end new-end))
           (go retry))))
    (values packet 0)))


(defun update-encoder (encoder &key bitrate signal)
  (cref:c-with ((err :int))
    (macrolet ((%update-encoder (request-code &rest values)
                 `(unless (= (%opus:encoder-ctl encoder ,request-code ,@values) 0)
                    (error "Failed to fulfill encoder request: ~A"
                           (cffi:foreign-string-to-lisp (%opus:strerror err))))))
      (when bitrate
        (%update-encoder %opus:+set-bitrate-request+ :int bitrate))
      (when signal
        (%update-encoder %opus:+set-signal-request+
                         :int (ecase signal
                                (:auto %opus:+auto+)
                                (:voice %opus:+signal-voice+)
                                (:music %opus:+signal-music+)))))))

(defun encode-frame (stream
                     encoder
                     frame
                     samples-per-channel
                     &key packet-buffer)
  "
   * FRAME '(signed-byte 16) static-vector with samples for duration of 2.5, 5, 10, 20, 40 or 60 ms.
   * PACKET-BUFFER '(unsigned-byte 8) static-vector. Recommended size is 4KiB-1.
"
  (let* ((bytes-written (%opus:encode encoder
                                      (sv:static-vector-pointer frame)
                                      samples-per-channel
                                      (sv:static-vector-pointer packet-buffer)
                                      (length packet-buffer))))
    (when (< bytes-written 0)
      (error "Failed to encode audio: ~A"
             (cffi:foreign-string-to-lisp (%opus:strerror bytes-written))))
    (encode-packet stream packet-buffer :end bytes-written))
  (values))


(defun clean-frame-buffer-tail (frame-buffer head-sample-count)
  (host:memset (cffi:inc-pointer (sv:static-vector-pointer frame-buffer)
                                 (* 2 head-sample-count))
               0
               (* 2 (- (length frame-buffer) head-sample-count))))


(defun encode-audio (frame-stream-in packet-stream-out
                     frame-size
                     sample-rate
                     channels
                     &key (application :auto)
                       ((:signal signal) :auto)
                       (bitrate 96000))
  "
  * FRAME-SIZE - number of samples in the whole frame (all channels).
"
  (cref:c-with ((err :int))
    (let* ((*length-buffer* (make-array 8 :element-type '(unsigned-byte 8)))
           (application (ecase application
                          ((:auto :audio) %opus:+application-audio+)
                          (:voice %opus:+application-voip+)))
           (encoder (%opus:encoder-create sample-rate channels application (err &)))
           (samples-per-channel (/ frame-size channels)))
      (unless (= err 0)
        (error "Failed to create encoder (~A): ~A" err
               (cffi:foreign-string-to-lisp (%opus:strerror err))))
      (unwind-protect
           (sv:with-static-vectors ((frame-buffer frame-size
                                                  :element-type '(signed-byte 16))
                                    (packet-buffer +recommended-packet-byte-size+
                                                   :element-type '(unsigned-byte 8)))
             (update-encoder encoder
                             :bitrate bitrate
                             :signal signal)
             (loop for samples-read = (read-sequence frame-buffer frame-stream-in)
                   when (< samples-read frame-size)
                     do (clean-frame-buffer-tail frame-buffer samples-read)
                   do (encode-frame packet-stream-out
                                    encoder
                                    frame-buffer
                                    samples-per-channel
                                    :packet-buffer packet-buffer)
                   while (= samples-read frame-size)))
        (%opus:encoder-destroy encoder)))))


(defun decode-frame (packet-stream
                     decoder
                     samples-per-channel
                     &key frame-buffer
                       packet-buffer)
  (multiple-value-bind (packet packet-size)
      (decode-packet packet-stream packet-buffer)
    (if (zerop packet-size)
        0
        (%opus:decode decoder
                      (sv:static-vector-pointer packet)
                      packet-size
                      (sv:static-vector-pointer frame-buffer)
                      samples-per-channel
                      0))))


(defun decode-audio (packet-stream-in frame-stream-out
                     sample-rate
                     channels)
  "
  * PACKET-STREAM-IN - input stream of type (UNSIGNED-BYTE 8)
  * FRAME-STREAM-OUT - output stream of type (SIGNED-BYTE 16)
"
  (cref:c-with ((err :int))
    (let ((decoder (%opus:decoder-create sample-rate channels (err &)))
          (*length-buffer* (make-array 8 :element-type '(unsigned-byte 8))))
      (unless (= err 0)
        (error "Failed to create decoder (~A): ~A" err
               (cffi:foreign-string-to-lisp (%opus:strerror err))))

      (unwind-protect
           (let ((max-samples-per-channel (* (/ sample-rate 1000) 60)))
             (sv:with-static-vectors ((frame-buffer (* max-samples-per-channel
                                                       channels)
                                                    :element-type '(signed-byte 16))
                                      (packet-buffer +recommended-packet-byte-size+
                                                     :element-type '(unsigned-byte 8)))
               (loop for samples-per-channel = (decode-frame packet-stream-in
                                                             decoder
                                                             max-samples-per-channel
                                                             :frame-buffer frame-buffer
                                                             :packet-buffer packet-buffer)
                     until (zerop samples-per-channel)
                     do (write-sequence frame-buffer frame-stream-out
                                        :end (* samples-per-channel channels)))))
        (%opus:decoder-destroy decoder)))))


(defun encode (source destination &key
                                    (sample-rate 48000)
                                    (channels 2))
  (with-open-file (in source :element-type '(signed-byte 16) :direction :input)
    (with-open-file (out destination :element-type '(unsigned-byte 8) :direction :output)
      (let ((frame-duration 20)) ;; msec
        (encode-audio in out (* (/ sample-rate 1000) frame-duration channels)
                      sample-rate
                      channels)))))


(defun decode (location &key
                          (sample-rate 48000)
                          (channels 2))
  (with-open-file (in location :element-type '(unsigned-byte 8) :direction :input)
    (flexi-streams:with-output-to-sequence (out :element-type '(signed-byte 16))
      (decode-audio in out sample-rate channels))))
