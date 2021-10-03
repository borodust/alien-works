(cl:in-package :alien-works.tools.ui)


(declaim (special *ui-callback*))


(defclass ui ()
  ((engine :initarg :engine)
   (renderer :initarg :renderer)
   (view :initarg :view)
   (callback :initarg :callback)
   (imgui-helper :initarg :imgui)
   (mouse-state :initform (host:make-mouse-state))
   (keyboard-modifier-state :initform (host:make-keyboard-modifier-state))))



(%ui:define-ui-callback ui-callback ()
  (funcall *ui-callback*))


(defun make-ui (engine)
  (let* ((engine-handle (%alien-works.graphics:handle-of engine))
         (view (%fm:create-view engine-handle))
         (helper (%ui:make-imgui-helper engine-handle view "")))

    (%ui:with-io (io)
      (%ui:init-io io))

    (make-instance 'ui :engine engine
                       :renderer (slot-value engine 'alien-works.graphics::renderer)
                       :view view
                       :callback (iffi:make-intricate-callback 'ui-callback)
                       :imgui helper)))


(defun destroy-ui (ui)
  (with-slots (engine imgui-helper view callback) ui
    (%ui:destroy-imgui-helper imgui-helper)
    (let ((engine-handle (%alien-works.graphics:handle-of engine)))
      (%fm:destroy-view engine-handle view))
    (%ui:destroy-ui-callback 'ui-callback callback)))


(defun update-ui-input (ui)
  (with-slots (mouse-state) ui
    (host:mouse-state mouse-state)
    (%ui:with-io (io)
      (%ui:update-mouse-position io
                                 (1- (host:mouse-state-x mouse-state))
                                 (1- (host:mouse-state-y mouse-state))))))


(defun handle-ui-event (ui event)
  (with-slots (keyboard-modifier-state) ui
    (host:keyboard-modifier-state keyboard-modifier-state)
    (%ui:with-io (io)
      (case (host:event-type event)
        (:textinput
         (%ui:add-input-characters io (%alien-works.host:event-input-foreign-text event)))
        ((:keyup :keydown)
         (%ui:update-keyboard-buttons io (host:scancode (host:event-key-scan-code event))
                                      (eq (host:event-type event) :keydown)
                                      (host:keyboard-modifier-state-some-pressed-p
                                       keyboard-modifier-state
                                       :lshift :rshift)
                                      (host:keyboard-modifier-state-some-pressed-p
                                       keyboard-modifier-state
                                       :lalt :ralt)
                                      (host:keyboard-modifier-state-some-pressed-p
                                       keyboard-modifier-state
                                       :lctrl :rctrl)
                                      (host:keyboard-modifier-state-some-pressed-p
                                       keyboard-modifier-state
                                       :lgui :rgui)))
        ((:mousebuttondown :mousebuttonup)
         (let ((button (host:event-mouse-button event))
               (pressed (eq (host:event-type event) :mousebuttondown)))
           (macrolet ((pressed-p (btn)
                        `(and pressed (eq button ,btn))))
             (%ui:update-mouse-buttons io
                                       (pressed-p :left)
                                       (pressed-p :right)
                                       (pressed-p :middle)))))
        (:mousewheel
         (multiple-value-bind (y-offset x-offset) (host:event-mouse-wheel event)
           (%ui:update-mouse-wheel io y-offset x-offset)))))))


(defun render-ui (ui width height time-delta ui-callback)
  (with-slots (view imgui-helper callback renderer) ui
    (%fm:update-view-viewport view 0 0 width height)
    (%ui:update-display-size imgui-helper width height 1f0 1f0)
    (let ((*ui-callback* ui-callback))
      (%ui:render-imgui imgui-helper callback time-delta))
    (%fm:render-view renderer view)))


(defmacro ui ((ui width height time-delta) &body body)
  `(render-ui ,ui ,width ,height ,time-delta
              (lambda () ,@body)))
