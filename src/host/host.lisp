(cl:in-package :alien-works.host)


(defvar *init-hooks* nil)

(a:define-constant +buffer-size+ 4096)
(a:define-constant +max-controller-guid-length+ 128)

(declaim (special *event*
                  *native-graphics-context*
                  *window*))


(defun sdl-error ()
  (cffi:foreign-string-to-lisp (%sdl:get-error)))

(cffi:defcfun ("SDL_GetModState" sdl-get-mod-state) :unsigned-int)

(u:define-enumval-extractor %scancode %sdl:scancode)
(u:define-enumbit-combiner key-modifier %sdl:keymod)
(u:define-enumval-extractor controller-button %sdl:game-controller-button)


(defun %host:get-clipboard-foreign-text ()
  (%sdl:get-clipboard-text))


(defun %host:set-clipboard-foreign-text (foreign-string)
  (%sdl:set-clipboard-text foreign-string))

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
                          (:input "rb")
                          (:output "wb")
                          (:append "ab"))))
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


(defun %read-host-file-into-vector (location &key ((:into provided-vector))

                                               offset ((:size provided-size))
                                               element-type
                                               constructor
                                               destructor
                                               call-with-pointer)
  (when (and element-type
             provided-vector
             (not (subtypep element-type (array-element-type provided-vector))))
    (error ":INTO array and :ELEMENT-TYPE are not compatible"))
  (let ((element-type (or (and provided-vector
                               (array-element-type provided-vector))
                          element-type
                          '(unsigned-byte 8))))
    (unless (and (listp element-type)
                 (member (first element-type) '(unsigned-byte signed-byte))
                 (member (second element-type) '(8 16 32 64)))
      (error "Element type of static-vector must be either unsigned-byte or signed-byte of size 8, 16, 32 or 64"))
    (when (and provided-vector
               provided-size
               (> provided-size (length provided-vector)))
      (error "Provided size is smaller than length of provided static-vector"))
    (let ((file (%sdl:rw-from-file (namestring location) "rb")))
      (when (cffi:null-pointer-p file)
        (error "Failed to open ~A: ~A" location (sdl-error)))
      (unwind-protect
           (let* ((file-size (%sdl:r-wseek file 0 %sdl:+rw-seek-end+))
                  (offset (if (> file-size 0)
                              (mod (or offset 0) file-size)
                              0))
                  (rest-file-size (- file-size offset))
                  (calculated-size
                    (min rest-file-size
                         (or provided-size rest-file-size)
                         (or (and provided-vector (length provided-vector))
                             rest-file-size))))
             (when (< file-size 0)
               (error "Failed to lookup ~A size: ~A" location (sdl-error)))
             (when (> (+ offset (or provided-size 0)) file-size)
               (error "Sum of offset and provided size is greater than size of the ~A: got ~A, expected no more than ~A"
                      location (+ offset calculated-size) file-size))
             (%sdl:r-wseek file offset %sdl:+rw-seek-set+)
             (let* ((out (if provided-vector
                             provided-vector
                             (funcall constructor calculated-size :element-type element-type)))
                    (objects-read (funcall call-with-pointer out
                                           (lambda (ptr)
                                             (%sdl:r-wread file ptr calculated-size 1)))))
               (unless (= objects-read 1)
                 (unless provided-vector
                   (funcall destructor out))
                 (error "Failed to read ~A of size ~A: ~A" location file-size (sdl-error)))
               (values out file-size)))
        (%sdl:r-wclose file)))))


(defun read-host-file-into-static-vector (location &key ((:into provided-static-vector))
                                                     offset ((:size provided-size))
                                                     element-type)
  (%read-host-file-into-vector location
                               :into provided-static-vector
                               :offset offset
                               :size provided-size
                               :element-type element-type
                               :constructor #'sv:make-static-vector
                               :destructor #'sv:free-static-vector
                               :call-with-pointer (lambda (vec call-with-pointer)
                                                    (funcall call-with-pointer
                                                             (sv:static-vector-pointer vec)))))


(defun read-host-file-into-shareable-vector (location &key ((:into provided-shareable-vector))
                                                        offset ((:size provided-size)))
  (%read-host-file-into-vector location
                               :into provided-shareable-vector
                               :offset offset
                               :size provided-size
                               :element-type '(unsigned-byte 8)
                               :constructor (lambda (size &rest args)
                                              (declare (ignore args))
                                              (cffi:make-shareable-byte-vector size))
                               :destructor (lambda (vec) (declare (ignore vec)))
                               :call-with-pointer (lambda (vec call-with-pointer)
                                                    (cffi:with-pointer-to-vector-data (ptr vec)
                                                      (funcall call-with-pointer ptr)))))

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
(declaim (special *primary* *secondary* *root* *root-window*))

(u:define-enumval-extractor gl-attr %sdl:g-lattr)
(u:define-enumval-extractor gl-profile %sdl:g-lprofile)
(u:define-enumbit-combiner gl-context-flags %sdl:g-lcontext-flag)
(u:define-enumbit-combiner window-flags %sdl:window-flags)


(defun setup-opengl-version (major minor)
  (%sdl:gl-set-attribute (gl-attr :context-major-version) major)
  (%sdl:gl-set-attribute (gl-attr :context-minor-version) minor)
  (let ((win (%sdl:create-window "SETUP"
                                 %sdl:+windowpos-undefined+
                                 %sdl:+windowpos-undefined+
                                 1 1
                                 (window-flags :opengl :hidden))))
    (unless (cffi:null-pointer-p win)
      (let ((ctx (%sdl:gl-create-context win)))
        (unwind-protect
             (unless (cffi:null-pointer-p ctx)
               (prog1 t
                 (%sdl:gl-delete-context ctx)))
          (%sdl:destroy-window win))))))


(defun setup-most-recent-opengl-es-context ()
  (%sdl:gl-set-attribute (gl-attr :context-profile-mask)
                         (gl-profile :es))
  (loop for (major minor) in '((3 2) (3 1) (3 0))
          thereis (setup-opengl-version major minor)
        finally (error "Required OpenGL ES version is not available (3.0+)")))


(defun setup-most-recent-opengl-context ()
  (%sdl:gl-set-attribute (gl-attr :context-profile-mask)
                         (gl-profile :core))
  (loop for (major minor) in '((4 6) (4 5) (4 3) (4 1))
          thereis (setup-opengl-version major minor)
        finally (error "Required OpenGL version is not available (4.1+)")))


(defun %host:window-graphics-context ()
  *native-graphics-context*)


(defun ensure-root-context ()
  (unless (= (%sdl:gl-make-current *root-window* *root*) 0)
    (error "Failed to make root GL context current")))


(defun call-with-window (callback &key title)
  (%sdl:set-main-ready)
  (unless (zerop (%sdl:init (logior %sdl:+init-timer+
                                    %sdl:+init-video+
                                    %sdl:+init-gamecontroller+
                                    %sdl:+init-haptic+)))
    (error "Failed to initialize SDL"))
  (%init-host)

  (%sdl:gl-set-attribute (gl-attr :share-with-current-context) 1)
  ;; TODO: move filament package somewhere else, probably
  (cref:c-with ((pf (:struct %filament.extra:pixel-format)))
    (%filament.extra:select-pixel-format (pf &))

    (%sdl:gl-set-attribute (gl-attr :red-size) (pf :red-bits))
    (%sdl:gl-set-attribute (gl-attr :green-size) (pf :green-bits))
    (%sdl:gl-set-attribute (gl-attr :blue-size) (pf :blue-bits))
    (%sdl:gl-set-attribute (gl-attr :alpha-size) (pf :alpha-bits))

    (%sdl:gl-set-attribute (gl-attr :accum-red-size) (pf :accum-red-bits))
    (%sdl:gl-set-attribute (gl-attr :accum-green-size) (pf :accum-green-bits))
    (%sdl:gl-set-attribute (gl-attr :accum-blue-size) (pf :accum-blue-bits))
    (%sdl:gl-set-attribute (gl-attr :accum-alpha-size) (pf :accum-alpha-bits))

    (%sdl:gl-set-attribute (gl-attr :buffer-size) (pf :buffer-bits))
    (%sdl:gl-set-attribute (gl-attr :depth-size) (pf :depth-bits))
    (%sdl:gl-set-attribute (gl-attr :stencil-size) (pf :stencil-bits)))
  (%sdl:gl-set-attribute (gl-attr :doublebuffer) 1)

  (if (uiop:featurep :android)
      (setup-most-recent-opengl-es-context)
      (setup-most-recent-opengl-context))

  (let ((window (cffi:with-foreign-string (name (or title "ALIEN-WORKS"))
                  (%sdl:create-window name
                                      %sdl:+windowpos-undefined+
                                      %sdl:+windowpos-undefined+
                                      1280 960
                                      (apply #'window-flags
                                             :opengl
                                             :allow-highdpi
                                             :shown
                                             (append
                                              (when (uiop:featurep :android)
                                                (list :fullscreen)))))))
        (*root-window* (%sdl:create-window "HIDDEN_ROOT"
                                           %sdl:+windowpos-undefined+
                                           %sdl:+windowpos-undefined+
                                           1 1
                                           (window-flags :opengl :hidden))))
    (when (cffi:null-pointer-p window)
      (error "Failed to create main window"))
    (when (cffi:null-pointer-p *root-window*)
      (error "Failed to create hidden root window"))

    (let ((*root* (%sdl:gl-create-context *root-window*))
          (*primary* (%sdl:gl-create-context window)))
      (ensure-root-context)
      (unwind-protect
           (cref:c-with ((event %sdl:event))
             (let* ((*event* (event &))
                    (*native-graphics-context* (%native-gl-context *primary*))
                    (*window* window))
               (funcall callback)))
        (%sdl:gl-delete-context *primary*)
        (%sdl:gl-delete-context *root*)
        (%sdl:destroy-window window)
        (%sdl:quit)))))


(defmacro %host:with-window ((&key title) &body body)
  `(call-with-window (lambda ()
                       ,@body)
                     ,@(when title
                         `(:title ,title))))


(defun %host:make-shared-context-thread (action)
  (let* ((win (%sdl:create-window "SECONDARY"
                                  %sdl:+windowpos-undefined+
                                  %sdl:+windowpos-undefined+
                                  1 1
                                  (window-flags :opengl :hidden)))
         (ctx (%sdl:gl-create-context win)))
    (ensure-root-context)
    (bt:make-thread
     (lambda ()
       (unwind-protect
            (let ((*native-graphics-context* (%native-gl-context ctx)))
              (unless (= (%sdl:gl-make-current win ctx) 0)
                (error "Failed to make secondary context current: ~A" (sdl-error)))
              (format t "~%Shared context thread GL: ~{~A~^; ~}"
                      (list
                       (gl:get-string :version)
                       (gl:get-string :renderer)
                       (gl:get-string :vendor)))
              (finish-output)
              (funcall action))
         (%sdl:gl-delete-context ctx)
         (%sdl:destroy-window win)))
     :name "shared-context-thread")))


(defun %host:window-surface ()
  (cref:c-with ((wm-info %sdl:sys-w-minfo))
    (setf (wm-info :version :major) %sdl:+major-version+
          (wm-info :version :minor) %sdl:+minor-version+
          (wm-info :version :patch) %sdl:+patchlevel+)

    (%sdl:get-window-wm-info *window* (wm-info &))
    (%window-surface (wm-info &))))


(defun window-width ()
  (cref:c-with ((width :int))
    (%sdl:get-window-size *window* (width &) (cffi:null-pointer))
    width))


(defun window-height ()
  (cref:c-with ((height :int))
    (%sdl:get-window-size *window* (cffi:null-pointer) (height &))
    height))


(defun framebuffer-width ()
  (cref:c-with ((width :int))
    (%sdl:gl-get-drawable-size *window* (width &) (cffi:null-pointer))
    width))


(defun framebuffer-height ()
  (cref:c-with ((height :int))
    (%sdl:gl-get-drawable-size *window* (cffi:null-pointer) (height &))
    height))


(defun window-display ()
  (make-display (%sdl:get-window-display-index *window*)))


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
    (case type
      (:keydown :keyboard-button-down)
      (:keyup :keyboard-button-up)

      (:mousebuttondown :mouse-button-down)
      (:mousebuttonup :mouse-button-up)
      (:mousewheel :mouse-wheel)
      (:mousemotion :mouse-motion)

      (:joyaxismotion :game-controller-axis-motion)
      (:joyballmotion :game-controller-ball-motion)
      (:joyhatmotion :game-controller-hat-motion)
      (:joybuttonup :game-controller-button-up)
      (:joybuttondown :game-controller-button-down)
      (:joydeviceadded :game-controller-added)
      (:joydeviceremoved :game-controller-removed)

      (:audiodeviceadded :audio-device-added)
      (:audiodeviceremoved :audio-device-removed)

      (:controlleraxismotion :gamepad-axis-motion)
      (:controllerbuttondown :gamepad-button-down)
      (:controllerbuttonup :gamepad-button-up)
      (:controllerdeviceadded :gamepad-added)
      (:controllerdeviceremoved :gamepad-removed)
      (:controllerdeviceremapped :gamepad-remapped)

      (:dollargesture :$-gesture)
      (:dollarrecord :$-gesture-record)

      (:dropfile :drop-file)
      (:droptext :drop-text)
      (:dropbegin :drop-begin)
      (:dropcomplete :drop-complete)

      (:fingermotion :finger-motion)
      (:fingerdown :finger-down)
      (:fingerup :finger-up)

      (:syswmevent :sys-wm)

      (:textediting :text-edit)
      (:textinput :text-input)
      (:windowevent :window)
      (:multigesture :simple-gesture)

      ((:quit) type)

      (otherwise :unknown))))


(defstruct mouse-state
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (buttons 0 :type fixnum))


(defun mouse-state (&optional mouse-state)
  (let ((mouse-state (if mouse-state mouse-state (make-mouse-state))))
    (cref:c-with ((x :int)
                  (y :int))
      (setf (mouse-state-buttons mouse-state) (%sdl:get-mouse-state (x &) (y &))
            (mouse-state-x mouse-state) x
            (mouse-state-y mouse-state) y))
    mouse-state))


(defun mouse-state-left-button-pressed-p (state)
  (/= (logand (mouse-state-buttons state) %sdl:+button-lmask+) 0))


(defun mouse-state-right-button-pressed-p (state)
  (/= (logand (mouse-state-buttons state) %sdl:+button-rmask+) 0))


(defun mouse-state-middle-button-pressed-p (state)
  (/= (logand (mouse-state-buttons state) %sdl:+button-mmask+) 0))


(defun %host:event-input-foreign-text (event)
  (cref:c-ref event %sdl:event :text :text &))


(defmacro define-event-accessor (name type (event &rest lambda-list) &body body)
  (multiple-value-bind (doc body)
      (if (stringp (first body))
          (values (first body) (rest body))
          (values nil body))
    `(defun ,name (,event ,@lambda-list)
       ,@(when doc `(,doc))
       (when (member (event-type ,event) '(,@(a:ensure-list type)))
         ,@body))))


(define-event-accessor event-mouse-button (:mouse-button-up :mouse-button-down) (event)
  (case (cref:c-ref event %sdl:event :button :button)
    (#.%sdl:+button-left+ :left)
    (#.%sdl:+button-right+ :right)
    (#.%sdl:+button-middle+ :middle)
    (otherwise nil)))


(define-event-accessor event-mouse-position (:mouse-motion) (event)
  (cref:c-val ((event %sdl:event))
    (values (event :motion :x) (event :motion :y))))


(define-event-accessor event-mouse-wheel :mouse-wheel (event)
  (cref:c-val ((event %sdl:event))
    (values (event :wheel :y) (event :wheel :x))))


(u:define-case-converter (keymod host)
  (:lalt :left-alt)
  (:ralt :right-alt)
  (:lshift :left-shift)
  (:rshift :right-shift)
  (:lctrl :left-ctrl)
  (:rctrl :right-ctrl)
  (:lgui :left-super)
  (:rgui :right-super)
  (:gui :super)
  (:capslock :caps-lock))


(defun scancode (value)
  (%scancode (host->keymod value)))


(define-event-accessor event-key-scan-code (:keyboard-button-down :keyboard-button-up) (event)
  (keymod->host (cref:c-ref event %sdl:event :key :keysym :scancode)))


(define-event-accessor event-game-controller-id (:game-controller-button-down
                                                 :game-controller-button-up)
    (event)
  (cref:c-ref event %sdl:event :jbutton :which))


(define-event-accessor event-game-controller-button (:game-controller-button-down
                                                     :game-controller-button-up)
    (event)
  (cref:c-ref event %sdl:event :jbutton :button))


(define-event-accessor event-gamepad-id (:gamepad-button-down :gamepad-button-up) (event)
  (cref:c-ref event %sdl:event :cbutton :which))


(u:define-case-converter (controller-button host)
  (:leftstick :left-stick)
  (:rightstick :right-stick)
  (:leftshoulder :left-shoulder)
  (:rightshoulder :right-shoulder))


(u:define-case-converter (controller-axis host)
  (:left-x :leftx)
  (:left-y :lefty)
  (:right-x :rightx)
  (:right-y :righty)
  (:trigger-left :triggerleft)
  (:trigger-right :triggerright))


(define-event-accessor event-gamepad-button (:gamepad-button-down :gamepad-button-up) (event)
  (controller-button->host
   (controller-button
    (cref:c-ref event %sdl:event :cbutton :button))))


(defstruct keyboard-modifier-state
  (buttons 0 :type fixnum))


(defun keyboard-modifier-state (&optional keyboard-modifier-key-state)
  (let ((state (or keyboard-modifier-key-state (make-keyboard-modifier-state))))
    (setf (keyboard-modifier-state-buttons state) (sdl-get-mod-state))
    state))


(defun keyboard-modifier-state-pressed-p (state &rest modifiers)
  (/= 0 (logand (keyboard-modifier-state-buttons state)
                (apply #'key-modifier modifiers))))


(define-compiler-macro keyboard-modifier-state-some-pressed-p (state &rest modifiers)
  `(/= 0 (logand (keyboard-modifier-state-buttons ,state)
                 (key-modifier ,@(loop for mod in modifiers
                                       collect `(host->keymod ,mod))))))


;;;
;;; GAME CONTROLLERS
;;;
(defmacro do-game-controller-ids ((game-controller-id) &body body)
  `(loop for ,game-controller-id from 0 below (%sdl:num-joysticks)
         do (progn ,@body)))


(defun game-controller-gamepad-p (game-controller-id)
  (%sdl:is-game-controller game-controller-id))


(defun game-controller-name-by-id (game-controller-id)
  (%sdl:joystick-name-for-index game-controller-id))


(defun game-controller-name (game-controller)
  (%sdl:joystick-name game-controller))


(defun game-controller-id (game-controller)
  (%sdl:joystick-instance-id game-controller))


(defun grab-game-controller (game-controller-id)
  (%sdl:joystick-open game-controller-id))


(defun release-game-controller (game-controller)
  (%sdl:joystick-close game-controller))


(defun game-controller-guid (game-controller)
  (cref:c-with ((str :char :count (1+ +max-controller-guid-length+)))
    (setf (str +max-controller-guid-length+) 0)
    (%sdl:joystick-get-guid-string game-controller (str &) +max-controller-guid-length+)
    (cffi:foreign-string-to-lisp str)))


(defun game-controller-attached-p (game-controller)
  (%sdl:joystick-get-attached game-controller))


(defun game-controller-axes-count (game-controller)
  (%sdl:joystick-num-axes game-controller))


(defun game-controller-axis-short-value (game-controller axis-id)
  (%sdl:joystick-get-axis game-controller axis-id))


(defun game-controller-axis-float-value (game-controller axis-id)
  (let ((value (game-controller-axis-short-value game-controller axis-id)))
    (if (< value 0)
        (/ value 32768f0)
        (/ value 32767f0))))


(defun game-controller-button-count (game-controller)
  (%sdl:joystick-num-axes game-controller))


(defun game-controller-button-pressed-p (game-controller button-id)
  (/= 0 (%sdl:joystick-get-button game-controller button-id)))


(defun game-controller-ball-count (game-controller)
  (%sdl:joystick-num-balls game-controller))


(defun game-controller-ball-value (game-controller ball-id result-vec2)
  (%sdl:joystick-get-ball game-controller ball-id
                          (%math:vec2-element-ptr result-vec2 0)
                          (%math:vec2-element-ptr result-vec2 1))
  result-vec2)


(defun game-controller-hat-count (game-controller)
  (%sdl:joystick-num-hats game-controller))


(u:define-case-converter (hat host :otherwise :unknown)
  (#.%sdl:+hat-centered+ :centered)
  (#.%sdl:+hat-up+ :up)
  (#.%sdl:+hat-right+ :right)
  (#.%sdl:+hat-down+ :down)
  (#.%sdl:+hat-left+ :left)
  (#.%sdl:+hat-rightup+ :right-up)
  (#.%sdl:+hat-rightdown+ :right-down)
  (#.%sdl:+hat-leftup+ :left-up)
  (#.%sdl:+hat-leftdown+ :left-down))


(defun game-controller-hat-value (game-controller hat-id)
  (hat->host (%sdl:joystick-get-hat game-controller hat-id)))


(defun game-controller-power-level (game-controller)
  (%sdl:joystick-current-power-level game-controller))


(defun game-controller-haptic-p (game-controller)
  (/= (%sdl:joystick-is-haptic game-controller) 0))

;;;
;;; GAMEPADS
;;;
(defun load-gamepad-mappings-from-host-file (host-file)
  (%sdl:game-controller-add-mappings-from-rw (%handle-of host-file) 0))


(defmacro do-gamepad-ids ((gamepad-id) &body body)
  `(do-game-controller-ids (,gamepad-id)
     (when (game-controller-gamepad-p ,gamepad-id)
       ,@body)))


(defun gamepad-name-by-id (gamepad-id)
  (%sdl:game-controller-name-for-index gamepad-id))


(defun grab-gamepad (gamepad-id)
  (%sdl:game-controller-open gamepad-id))


(defun release-gamepad (gamepad)
  (%sdl:game-controller-close gamepad))


(defun gamepad-power-level (gamepad)
  (game-controller-power-level (gamepad-controller gamepad)))


(defun gamepad-name (gamepad)
  (%sdl:game-controller-name gamepad))


(defun gamepad-controller (gamepad)
  (%sdl:game-controller-get-joystick gamepad))


(defun gamepad-attached-p (gamepad)
  (%sdl:game-controller-get-attached gamepad))


(defun gamepad-button-pressed-p (gamepad button)
  ":a :b :x :y :back :guide :start :leftstick :right-stick :left-shoulder :right-shoulder
:dpad-up :dpad-down :dpad-left :dpad-right :misc1 :paddle1 :paddle2 :paddle3 :paddle4 :touchpad"
  (/= 0 (%sdl:game-controller-get-button gamepad
                                         (host->controller-button button))))


(defun gamepad-axis-short-value (gamepad axis)
  ":left-x :left-y :right-x :right-y :trigger-left :trigger-right
Returns -32768 to 32767 for sticks and 0 to 32767 for triggers"
  (%sdl:game-controller-get-axis gamepad (controller-axis->host axis)))


(defun gamepad-axis-float-value (gamepad axis)
  "-1f0 to 1f0 for sticks and 0f0 to 1f0 for triggers"
  (let ((value (gamepad-axis-short-value gamepad axis)))
    (if (< value 0)
        (/ value 32768f0)
        (/ value 32767f0))))


(defun gamepad-haptic-p (gamepad)
  (game-controller-haptic-p (gamepad-controller gamepad)))


;;;
;;; HAPTIC
;;;
(defmacro do-haptic-device-ids ((device-id) &body body)
  `(loop for ,device-id from 0 below (%sdl:num-haptics)
         do (progn ,@body)))


(defun grab-haptic-device (device-id)
  (%sdl:haptic-open device-id))


(defun grab-game-controller-haptic-device (game-controller)
  (%sdl:haptic-open-from-joystick game-controller))


(defun grab-gamepad-haptic-device (gamepad)
  (grab-game-controller-haptic-device (gamepad-controller gamepad)))


(defun release-haptic-device (haptic-device)
  (%sdl:haptic-close haptic-device))


(defun make-constant-haptic-effect-configuration (&key)
  (cref:c-let ((effect %sdl:haptic-effect :alloc t))
    (memset (effect &) 0 (cffi:foreign-type-size '%sdl:haptic-effect))
    (setf (effect :type) %sdl:+haptic-constant+)
    effect))


(defun destroy-haptic-effect-configuration (haptic-effect-configuration)
  (cffi:foreign-free haptic-effect-configuration))


(defun haptic-effect-supported-p (haptic-device effect)
  (%sdl:haptic-effect-supported haptic-device effect))


(defun add-rumble (haptic-device)
  (zerop (%sdl:haptic-rumble-init haptic-device)))


(defun play-rumble (haptic-device strength length-ms)
  (%sdl:haptic-rumble-play haptic-device (float strength 0f0) (max 0 (floor length-ms)))
  (values))

;;;
;;; TOUCH
;;;
(define-event-accessor event-finger-id (:finger-down :finger-motion :finger-up) (event)
  (cref:c-ref event %sdl:event :tfinger :finger-id))


(define-event-accessor event-finger-x (:finger-down :finger-motion :finger-up) (event)
  (cref:c-ref event %sdl:event :tfinger :x))


(define-event-accessor event-finger-y (:finger-down :finger-motion :finger-up) (event)
  (cref:c-ref event %sdl:event :tfinger :y))


(define-event-accessor event-finger-position (:finger-down :finger-motion :finger-up) (event)
  (cref:c-val ((event %sdl:event))
    (values (cref:c-ref event %sdl:event :tfinger :x)
            (cref:c-ref event %sdl:event :tfinger :y))))


(define-event-accessor event-finger-x-offset :finger-motion (event)
  (cref:c-ref event %sdl:event :tfinger :dx))


(define-event-accessor event-finger-y-offset :finger-motion (event)
  (cref:c-ref event %sdl:event :tfinger :dy))


;;;
;;; SIMPLE GESTURE
;;;
(define-event-accessor event-simple-gesture-finger-count :simple-gesture (event)
  (cref:c-ref event %sdl:event :mgesture :num-fingers))


(define-event-accessor event-simple-gesture-distance-offset :simple-gesture (event)
  (cref:c-ref event %sdl:event :mgesture :d-dist))


(define-event-accessor event-simple-gesture-rotation-offset :simple-gesture (event)
  (cref:c-ref event %sdl:event :mgesture :d-theta))


(define-event-accessor event-simple-gesture-x :simple-gesture (event)
  (cref:c-ref event %sdl:event :mgesture :x))


(define-event-accessor event-simple-gesture-y :simple-gesture (event)
  (cref:c-ref event %sdl:event :mgesture :y))


;;;
;;; RUNNING
;;;
(defun provided-workdir ()
  (let ((workdir (uiop:getenv "ALIEN_WORKS_WORKDIR")))
    (unless (a:emptyp workdir)
      (uiop:ensure-directory-pathname workdir))))


(defun working-directory ()
  (or (provided-workdir)
      (a:when-let ((exec-path (first (uiop:raw-command-line-arguments))))
        exec-path)
      (uiop:getcwd)))


(defun add-known-foreign-library-directories ()
  (loop with workdir = (working-directory)
        with libpaths = (mapcar #'uiop:ensure-directory-pathname
                                (remove-if #'a:emptyp
                                           (uiop:split-string (uiop:getenv "ALIEN_WORKS_LIBRARY_PATH")
                                                              :separator ":")))
        for libdir in (nconc
                       (when workdir
                         (list (merge-pathnames "lib/" workdir)
                               (merge-pathnames "usr/lib/" workdir)
                               workdir))
                       (nreverse libpaths))
        do (pushnew libdir cffi:*foreign-library-directories* :test #'equal)))


(defun run ()
  (unwind-protect
       (progn
         (setf %gl:*gl-get-proc-address* #'%sdl:gl-get-proc-address)
         (u:reload-foreign-libraries)
         (cl-opengl-library:load-opengl-library)
         (loop with args = (uiop:command-line-arguments)
               for hook in *init-hooks*
               do (apply hook args)))
    (bodge-blobs-support:close-foreign-libraries)
    (u:unload-foreign-libraries)))


(defmacro definit (name (&rest lambda-list) &body body)
  (let ((initializer (a:symbolicate name '$alien-works-init)))
    `(progn
       (pushnew ',initializer *init-hooks*)
       (defun ,initializer ,@(if lambda-list
                                 `(,lambda-list)
                                 (a:with-gensyms (args-param)
                                   `((&rest ,args-param)
                                     (declare (ignore ,args-param)))))
         ,@body))))
