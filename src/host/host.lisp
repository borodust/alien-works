(cl:in-package :alien-works.host)


(defvar *init-hooks* nil)
(a:define-constant +buffer-size+ 4096)

(declaim (special *event*))


;;;
;;; DISPLAY
;;;
(defstruct (display
            (:constructor %make-display))
  name
  x
  y
  width
  height
  orientation)


(defun make-display (id)
  (cref:c-with ((rect %sdl:rect))
    (let (x y w h)
      (%sdl:get-display-bounds id (rect &))
      (setf w (rect :w)
            h (rect :h)
            x (rect :x)
            y (rect :y))
      (%make-display
       :name (cffi:foreign-string-to-lisp (%sdl:get-display-name id))
       :width w
       :height h
       :x x
       :y y
       :orientation (%sdl:get-display-orientation id)))))


(defun list-displays ()
  (loop for i from (%sdl:get-num-video-displays)
        collect (make-display i)))

;;;
;;; WINDOW
;;;
(defun call-with-window (callback)
  (%sdl:init %sdl:+init-video+)
  (%init-host)
  (let ((window (cffi:with-foreign-string (name "YO")
                  (%sdl:create-window name
                                      %sdl:+windowpos-undefined+
                                      %sdl:+windowpos-undefined+
                                      1280 960
                                      (cffi:foreign-enum-value '%sdl:window-flags :opengl)))))
    (when (cffi:null-pointer-p window)
      (error "Failed to create a window"))
    (unless (= (%sdl:gl-make-current window (cffi:null-pointer)) 0)
      (error "Failed to detach GL context"))
    (unwind-protect
         (cref:c-with ((event %sdl:event))
           (let ((*event* (event &)))
             (funcall callback window)))
      (%sdl:destroy-window window)
      (%sdl:quit))))


(defmacro with-window ((window) &body body)
  `(call-with-window (lambda (,window) ,@body)))


(defun window-surface (window)
  (cref:c-with ((wm-info %sdl:sys-w-minfo))
    (setf (wm-info :version :major) %sdl:+major-version+
          (wm-info :version :minor) %sdl:+minor-version+
          (wm-info :version :patch) %sdl:+patchlevel+)

    (%sdl:get-window-wm-info window (wm-info &))
    (%window-surface (wm-info &))))


(defun window-width (window)
  (cref:c-with ((width :int))
    (%sdl:get-window-size window (width &) (cffi:null-pointer))
    width))


(defun window-height (window)
  (cref:c-with ((height :int))
    (%sdl:get-window-size window (cffi:null-pointer) (height &))
    height))


(defun framebuffer-width (window)
  (cref:c-with ((width :int))
    (%sdl:gl-get-drawable-size window (width &) (cffi:null-pointer))
    width))


(defun framebuffer-height (window)
  (cref:c-with ((height :int))
    (%sdl:gl-get-drawable-size window (cffi:null-pointer) (height &))
    height))


(defun window-display (window)
  (make-display (%sdl:get-window-display-index window)))


;;;
;;; EVENTS
;;;
(defun handle-events (handler)
  (loop for result = (%sdl:poll-event *event*)
        while (> result 0)
        do (funcall handler *event*)))


(defun event-type (event)
  (let* ((id (cref:c-ref event %sdl:event :type))
         (type (cffi:foreign-enum-keyword '%sdl:event-type id :errorp nil)))
    (if type type :uknown)))


(defun run ()
  (loop with args = (uiop:command-line-arguments)
        for hook in *init-hooks*
        do (apply hook args)))


(defmacro definit (name (&rest lambda-list) &body body)
  (let ((initializer (a:symbolicate 'alien-works-init$ name)))
    `(progn
       (pushnew ',initializer *init-hooks*)
       (defun ,initializer ,@(if lambda-list
                                 `(,lambda-list)
                                 (a:with-gensyms (args-param)
                                   `((&rest ,args-param)
                                     (declare (ignore ,args-param)))))
         ,@body))))



;;;
;;;
;;;
(defun memcpy (destination source size)
  (%sdl:memcpy destination source size))


(defun memset (destination value size)
  (%sdl:memset destination value size))


;;;
;;; STREAMS
;;;
(defclass host-stream ()
  ((buffer :initform (sv:make-static-vector +buffer-size+ :element-type '(unsigned-byte 8))
           :reader %buffer-of)
   (handle :initarg :handle
           :reader %handle-of)
   (location :initarg :location)
   (size :initarg :size)))


(defun open-host-stream-from-location (location direction &optional size)
  (cond
    ((or (stringp location) (pathnamep location))
     (%sdl:rw-from-file (uiop:native-namestring location)
                        (ecase direction
                          (:input "r")
                          (:output "w")
                          (:append "a"))))
    ((cffi:pointerp location)
     (ecase direction
       (:input (%sdl:rw-from-const-mem location (truncate (or size 0))))
       ((:output :append) (%sdl:rw-from-mem location (truncate (or size 0))))))))


(defun reopen-host-stream-for-append (stream)
  (with-slots (handle location size) stream
    (%sdl:r-wclose handle)
    (setf handle (open-host-stream-from-location location :append size))))


(defmethod initialize-instance :after ((this host-stream) &key direction location size)
  (with-slots (handle) this
    (setf handle (open-host-stream-from-location location direction size))
    (when (cffi:null-pointer-p handle)
      (signal (make-condition 'file-error :pathname location)))))


(defmethod cl:close ((this host-stream) &key abort &allow-other-keys)
  (declare (ignore abort))
  (with-slots (handle buffer) this
    (when handle
      (%sdl:r-wclose handle)
      (sv:free-static-vector buffer)
      (setf handle nil
            buffer nil))))


(defclass host-input-stream (host-stream gray:fundamental-binary-input-stream) ())


(defmethod gray:stream-clear-input ((this host-input-stream))
  (declare (ignore this)))


(defmethod gray:stream-read-sequence ((this host-input-stream) sequence start end &key)
  (let ((total-size (- end start)))
    (when (< total-size 0)
      (error "End must be equal or greater than start"))
    (loop with bytes-left = total-size
          with bytes-read = 0
          for destination-idx = (+ start bytes-read)
          while (> bytes-left 0)
          do (let* ((step (min bytes-left +buffer-size+))
                    (read (%sdl:r-wread (%handle-of this)
                                        (sv:static-vector-pointer (%buffer-of this))
                                        1 step)))
               (incf bytes-read read)
               (if (< read step)
                   (setf bytes-left 0)
                   (decf bytes-left read))
               (unless (zerop read)
                 (replace sequence (%buffer-of this)
                          :start1 destination-idx :end1 (+ destination-idx read)
                          :start2 0 :end2 read)))
          finally (return bytes-read))))


(defmethod gray:stream-file-position ((this host-input-stream))
  (%sdl:r-wseek (%handle-of this) 0 %sdl:+rw-seek-cur+))


(defmethod (setf gray:stream-file-position) (value (this host-input-stream))
  (%sdl:r-wseek (%handle-of this) (truncate value) %sdl:+rw-seek-cur+))


(defmethod gray:stream-read-byte ((this host-input-stream))
  (let ((bytes-read (%sdl:r-wread (%handle-of this) (sv:static-vector-pointer (%buffer-of this)) 1 1)))
    (if (zerop bytes-read)
        :EOF
        (aref (%buffer-of this) 0))))


(defclass host-output-stream (host-stream gray:fundamental-binary-output-stream) ())


(defmethod gray:stream-write-byte ((this host-output-stream) byte)
  (setf (aref (%buffer-of this) 0) byte)
  (%sdl:r-wwrite (%handle-of this) (sv:static-vector-pointer (%buffer-of this)) 1 1)
  byte)


(defmethod gray:stream-write-sequence ((this host-output-stream) sequence start end &key)
  (let ((total-size (- end start)))
    (when (< total-size 0)
      (error "End must be equal or greater than start"))
    (loop with bytes-left = total-size
          with bytes-written = 0
          for source-idx = (+ start bytes-written)
          while (> bytes-left 0)
          do (let ((step (min bytes-left +buffer-size+)))
               (replace (%buffer-of this) sequence
                        :start1 0 :end1 step
                        :start2 source-idx :end2 (+ source-idx step))
               (%sdl:r-wwrite (%handle-of this)
                              (sv:static-vector-pointer (%buffer-of this))
                              1 step)
               (incf bytes-written step)
               (decf bytes-left step)))
    sequence))


(defmethod gray:stream-force-output ((this host-output-stream))
  (reopen-host-stream-for-append this))


(defmethod gray:stream-finish-output ((this host-output-stream))
  (reopen-host-stream-for-append this))


(defmethod gray:stream-clear-output ((this host-output-stream))
  (declare (ignore this)))


(defun open-host-file (location &key (direction :input) size)
  (ecase direction
    (:input (make-instance 'host-input-stream :location location :size size :direction :input))
    (:output (make-instance 'host-output-stream :location location :size size :direction :output))))


(defmacro with-open-host-file ((var location &rest keys) &body body)
  `(let ((,var (open-host-file ,location ,@keys)))
     (unwind-protect
          (progn ,@body)
       (close ,var))))
