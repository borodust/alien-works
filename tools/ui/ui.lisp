(cl:in-package :alien-works.tools.ui)


(declaim (special *ui-callback*
                  *ui-cleanup*))

(defstruct finger
  id
  (x 0)
  (y 0)
  (x-offset 0)
  (y-offset 0))


(defstruct gesture
  (finger-count 0)
  (distance 0)
  (rotation 0)
  (x nil)
  (y nil))


(defclass touch-mouse-emulation ()
  ((finger-table :initform (make-hash-table :test 'eql))
   (gesture :initform (make-gesture))

   (first-finger :initform nil)

   (left-pressed-p :initform nil :reader left-pressed-of)
   (right-pressed-p :initform nil :reader right-pressed-of)
   (middle-pressed-p :initform nil :reader middle-pressed-of)
   (x :initform nil :reader x-of)
   (y :initform nil :reader y-of)
   (x-scroll-prev :initform 0)
   (y-scroll-prev :initform 0)
   (x-scroll :initform 0)
   (y-scroll :initform 0)))


(defun read-touch-mouse-scroll (emulator)
  (with-slots (x-scroll y-scroll x-scroll-prev y-scroll-prev) emulator
    (let ((x (- x-scroll x-scroll-prev))
          (y (- y-scroll y-scroll-prev)))
      (setf x-scroll-prev x-scroll
            y-scroll-prev y-scroll)
      (values x y))))


(defun %update-touch-mouse-state (emulator)
  (with-slots (finger-table gesture first-finger
               left-pressed-p right-pressed-p middle-pressed-p
               x y x-scroll y-scroll x-scroll-prev y-scroll-prev)
      emulator
    (let ((finger-count (hash-table-count finger-table)))
      (cond
        ((and first-finger (zerop finger-count))
         (setf first-finger nil

               left-pressed-p nil
               middle-pressed-p nil
               right-pressed-p nil
               x-scroll 0
               y-scroll 0

               (gesture-finger-count gesture) 0
               (gesture-distance gesture) 0
               (gesture-rotation gesture) 0
               (gesture-x gesture) nil
               (gesture-y gesture) nil))
        ((null first-finger)
         (setf first-finger (first (a:hash-table-values finger-table)))))

      (when first-finger
        (cond
          ((= finger-count 1)
           (setf right-pressed-p nil
                 middle-pressed-p nil
                 left-pressed-p t
                 x-scroll 0
                 y-scroll 0
                 x-scroll-prev 0
                 y-scroll-prev 0))
          ((= finger-count 2)
           (setf right-pressed-p t
                 middle-pressed-p nil
                 left-pressed-p nil
                 x-scroll (finger-x-offset first-finger)
                 y-scroll (finger-y-offset first-finger)))
          ((= finger-count 3)
           (setf right-pressed-p nil
                 middle-pressed-p t
                 left-pressed-p nil
                 x-scroll 0
                 y-scroll 0
                 x-scroll-prev 0
                 y-scroll-prev 0))
          (t (setf right-pressed-p nil
                   middle-pressed-p nil
                   left-pressed-p nil
                   x-scroll 0
                   y-scroll 0
                   x-scroll-prev 0
                   y-scroll-prev 0)))

        (setf x (+ (finger-x first-finger) (finger-x-offset first-finger))
              y (+ (finger-y first-finger) (finger-y-offset first-finger)))))))


(defun update-touch-mouse-finger (emulator id pressing x y)
  (with-slots (finger-table) emulator
    (if pressing
        (let ((finger (gethash id finger-table)))
          (if finger
              (setf (finger-x finger) x
                    (finger-y finger) y)
              (setf (gethash id finger-table) (make-finger :id id
                                                           :x x
                                                           :y y))))
        (remhash id finger-table))
    (%update-touch-mouse-state emulator)))


(defun update-touch-mouse-motion (emulator id x-offset y-offset)
  (with-slots (finger-table) emulator
    (let ((finger (gethash id finger-table)))
      (incf (finger-x-offset finger) x-offset)
      (incf (finger-y-offset finger) y-offset))
    (%update-touch-mouse-state emulator)))


(defun update-touch-mouse-multigesture (emulator finger-count distance rotation x y)
  (with-slots (gesture) emulator
    (incf (gesture-distance gesture) distance)
    (incf (gesture-rotation gesture) rotation)
    (setf (gesture-finger-count gesture) finger-count
          (gesture-x gesture) x
          (gesture-y gesture) y)
    (%update-touch-mouse-state emulator)))


;;;
;;; UI
;;;
(defclass ui ()
  ((view :initarg :view)
   (callback :initarg :callback)
   (context :initarg :context)
   (imgui-helper :initarg :imgui)
   (width :initform 0 :initarg :width)
   (height :initform 0 :initarg :height)
   (framebuffer-width :initform 0 :initarg :framebuffer-width)
   (framebuffer-height :initform 0 :initarg :framebuffer-height)
   (scale :initform 0f0 :initarg :scale)
   (keyboard-modifier-state :initform (host:make-keyboard-modifier-state))
   (touch-mouse :initform (make-instance 'touch-mouse-emulation))))


(defun skip-ui-processing (&optional cleanup-function)
  (a:if-let (restart (find-restart 'skip-ui-processing))
    (invoke-restart restart cleanup-function)
    (error "No SKIP-UI-PROCESSING restart found")))


(define-condition unhandled-foreign-stack-condition (warning)
  ((instance :initarg :instance :reader unhandled-foreign-stack-condition-instance))
  (:report (lambda (c stream)
             (let ((condition (unhandled-foreign-stack-condition-instance c)))
               (format stream "Unhandled condition encountered during UI processing:~&~A"
                       condition)
               (format stream "~&~%Invoke #'alien-works:skip-ui-processing in your code to gracefully handle it.")))))


(defun skip-ui-rendering ()
  (throw 'skip-ui-rendering (values)))


(%ui:define-ui-callback ui-callback ()
  (block ui
    ;; prevent unwinding through foreign frames
    (let (registered-condition)
      (unwind-protect
           (restart-case
               (handler-bind ((serious-condition
                                (lambda (c)
                                  (setf registered-condition c))))
                 (funcall *ui-callback*))
             (skip-ui-processing (&optional cleanup-function)
               :report "Skip UI processing in foreign callback and call cleanup function in lisp environment."
               (setf registered-condition nil
                     *ui-cleanup* (or cleanup-function #'skip-ui-rendering))))
        (when registered-condition
          (warn (make-condition
                 'unhandled-foreign-stack-condition
                 :instance registered-condition)))
        ;; ensure we catch every stack unwinding and stop it
        ;; before foreign stack is reached
        (return-from ui)))))


(defun update-ui-size (ui width height
                        framebuffer-width framebuffer-height
                        scale)
  (with-slots (view context imgui-helper
               (this-width width)
               (this-height height)
               (this-framebuffer-width framebuffer-width)
               (this-framebuffer-height framebuffer-height)
               (this-scale scale))
      ui
    (%ui:with-bound-context (context)
      (unless (and (= width this-width)
                   (= height this-height)
                   (= framebuffer-width this-framebuffer-width)
                   (= framebuffer-height this-framebuffer-height))
        (let ((framebuffer-scale (float (/ framebuffer-width width) 0f0)))
          (%fm:update-view-viewport view
                                    0 0
                                    framebuffer-width
                                    framebuffer-height)
          (%ui:update-display-size imgui-helper
                                   width height
                                   framebuffer-scale
                                   framebuffer-scale)))
      (unless (= scale this-scale)
        (let ((adjusted-scale (float (* scale
                                        (/ width framebuffer-width))
                                     0f0))
              (prev-adjusted-scale (if (zerop this-framebuffer-width)
                                       1
                                       (float (* this-scale
                                                 (/ this-width this-framebuffer-width))
                                              0f0)))
              (style (%ui:style)))
          (%ui:scale-style style (* (/ 1 prev-adjusted-scale) adjusted-scale))
          (setf this-scale scale))))))


(defun make-ui (&key touch-padding)
  (let* ((engine-handle (%alien-works.graphics:engine-handle))
         (view (%fm:create-view engine-handle))
         (context (%ui:make-context))
         (helper (%ui:make-imgui-helper context engine-handle view "")))
    (%ui:with-bound-context (context)
      (%ui:initialize-context)

      (let ((style (%ui:style)))
        (when touch-padding
          (%ui:update-touch-padding style touch-padding touch-padding))))

    (make-instance 'ui :view view
                       :callback (iffi:make-intricate-callback 'ui-callback)
                       :imgui helper
                       :context context)))


(defun destroy-ui (ui)
  (with-slots (imgui-helper view callback) ui
    ;; context is destroyed in helper
    ;; see ImGuiHelper::~ImGuiHelper
    (%ui:destroy-imgui-helper imgui-helper)
    (%fm:destroy-view (%alien-works.graphics:engine-handle) view)
    (%ui:destroy-ui-callback 'ui-callback callback)))


(defun update-ui-input (ui)
  (with-slots (touch-mouse context) ui
    (%ui:with-bound-context (context)
      (flet ((%clamp (value)
               (cond
                 ((< -0.00001 value 0.00001) 0)
                 ((< value 0) -1/8)
                 (t 1/8))))
        (multiple-value-bind (x-scroll y-scroll)
            (read-touch-mouse-scroll touch-mouse)
          (%ui:update-mouse-wheel (%clamp y-scroll) (%clamp x-scroll)))))))


(defun update-input-from-touch (touch-mouse)
  (%ui:update-mouse-buttons (left-pressed-of touch-mouse)
                            (right-pressed-of touch-mouse)
                            (middle-pressed-of touch-mouse))
  (%ui:update-mouse-position (x-of touch-mouse)
                             (y-of touch-mouse)))


(defun handle-ui-event (ui event)
  (with-slots (keyboard-modifier-state touch-mouse context) ui
    (host:keyboard-modifier-state keyboard-modifier-state)
    (let ((width (alien-works:window-width))
          (height (alien-works:window-height)))
      (%ui:with-bound-context (context)
        (case (host:event-kind event)
          (:text-input
           (%ui:add-input-characters (%alien-works.host:event-input-foreign-text event)))
          ((:keyboard-button-up :keyboard-button-down)
           (%ui:update-keyboard-buttons (host:scancode (host:event-key-scan-code event))
                                        (eq (host:event-kind event) :keyboard-button-down)
                                        (host:keyboard-modifier-state-some-pressed-p
                                         keyboard-modifier-state :shift)
                                        (host:keyboard-modifier-state-some-pressed-p
                                         keyboard-modifier-state :alt)
                                        (host:keyboard-modifier-state-some-pressed-p
                                         keyboard-modifier-state :ctrl)
                                        (host:keyboard-modifier-state-some-pressed-p
                                         keyboard-modifier-state :super)))
          ((:mouse-button-down :mouse-button-up)
           (let ((button (host:event-mouse-button event))
                 (pressed (eq (host:event-kind event) :mouse-button-down)))
             (macrolet ((pressed-p (btn)
                          `(and pressed (eq button ,btn))))
               (%ui:update-mouse-buttons (pressed-p :left)
                                         (pressed-p :right)
                                         (pressed-p :middle)))))
          (:mouse-wheel
           (multiple-value-bind (y-offset x-offset) (host:event-mouse-wheel event)
             (%ui:update-mouse-wheel y-offset x-offset)))
          (:mouse-motion
           (multiple-value-bind (x y)
               (host:event-mouse-position event)
             (%ui:update-mouse-position (1- x) (1- y))))
          ((:finger-down :finger-up)
           (update-touch-mouse-finger touch-mouse
                                      (host:event-finger-id event)
                                      (not (eq (host:event-kind event) :finger-up))
                                      (* (host:event-finger-x event) width)
                                      (* (host:event-finger-y event) height))
           (update-input-from-touch touch-mouse))
          (:finger-motion
           (update-touch-mouse-motion touch-mouse
                                      (host:event-finger-id event)
                                      (* (host:event-finger-x-offset event) width)
                                      (* (host:event-finger-y-offset event) height))
           (update-input-from-touch touch-mouse))
          (:simple-gesture
           (update-touch-mouse-multigesture touch-mouse
                                            (host:event-simple-gesture-finger-count event)
                                            (host:event-simple-gesture-distance-offset event)
                                            (host:event-simple-gesture-rotation-offset event)
                                            (* (host:event-simple-gesture-x event) width)
                                            (* (host:event-simple-gesture-y event) height))
           (update-input-from-touch touch-mouse)))))))


(defun render-ui (ui time-delta ui-callback)
  (with-slots (view imgui-helper callback context) ui
    (catch 'skip-ui-rendering
      (%ui:with-bound-context (context)
        (let ((*ui-callback* ui-callback)
              (*ui-cleanup* nil))
          (%ui:render-imgui imgui-helper callback time-delta)
          (when *ui-cleanup*
            (funcall *ui-cleanup*)))
        (%fm:render-view (%alien-works.graphics:renderer-handle) view)))))


(defmacro ui ((ui time-delta) &body body)
  `(render-ui ,ui ,time-delta (lambda () ,@body)))


(defmacro rows (() &body rows)
  (let ((column-count (apply #'max (mapcar #'length rows))))
    (labels ((add-rest-columns (list count)
               (nconc list (loop repeat count
                                 collect '(%ui:next-column))))
             (process-columns (row)
               (loop with column-collected = 0
                     for element in row
                     collect (progn
                               (incf column-collected)
                               `(,@(if (null element)
                                       '(progn)
                                       `(unwind-protect ,element))
                                 (%ui:next-column)))
                       into elements
                     finally (return (add-rest-columns elements
                                                       (- column-count
                                                          column-collected))))))
      `(progn
         (%ui:columns ,column-count)
         (unwind-protect
              (progn
                ,@(loop for row in rows
                        append (process-columns row)))
           (%ui:columns 1))))))


(defun %add-font (ui data-ptr data-size pixel-size &key
                   oversample transfer-ownership)
  (with-slots (imgui-helper context) ui
    (%ui:with-bound-context (context)
      (prog1 (%ui:add-font-from-foreign data-ptr data-size
                                        (floor pixel-size)
                                        :transfer-ownership transfer-ownership
                                        :oversample oversample)
        (%ui:update-font-atlas imgui-helper
                               (%alien-works.graphics:engine-handle))))))


(defun add-font (ui data pixel-size &key oversample)
  (let* ((data-size (length data))
         (data-ptr (cffi:foreign-alloc :uint8
                                       :initial-contents data
                                       :count data-size)))
    (%add-font ui data-ptr data-size pixel-size
               :oversample oversample
               :transfer-ownership t)))



(defun use-memory-vector-font (ui memvec pixel-size &key oversample)
  (let* ((data-size (length memvec))
         (data-ptr (%mem:memory-vector-pointer memvec)))
    (%add-font ui data-ptr data-size pixel-size
               :oversample oversample
               :transfer-ownership nil)))
