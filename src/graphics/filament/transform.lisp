(cl:in-package :%alien-works.filament)


(defun transform-manager (engine)
  (%filament:get-transform-manager
   '(claw-utils:claw-pointer %filament:engine) engine))


(defmacro with-transform-instance ((instance entity) transform-manager &body body)
  `(iffi:with-intricate-instance (,instance %filament:transform-manager+instance)
     (%filament:get-instance
      :const
      '(claw-utils:claw-pointer %filament:transform-manager+instance) ,instance
      '(claw-utils:claw-pointer %filament:transform-manager) ,transform-manager
      '(claw-utils:claw-pointer %filament:utils+entity) ,entity)
     ,@body))


(defun (setf transform) (mat4f transform-manager entity-instance)
  (%filament:set-transform
   '(claw-utils:claw-pointer %filament:transform-manager) transform-manager
   '(claw-utils:claw-pointer %filament:transform-manager+instance) entity-instance
   '(claw-utils:claw-pointer %filament:math+mat4f) mat4f))


(defun attach-transform (transform-manager entity-instance)
  (iffi:with-intricate-instance (transform-instance %filament:transform-manager+instance)
    (%filament:create
     '(claw-utils:claw-pointer %filament:transform-manager) transform-manager
     '(claw-utils:claw-pointer %filament:utils+entity) entity-instance
     '(claw-utils:claw-pointer %filament:transform-manager+instance) transform-instance)))


(defun detach-transform (transform-manager entity-instance)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament:transform-manager) transform-manager
   '(claw-utils:claw-pointer %filament:utils+entity) entity-instance))


(defun (setf transform-parent) (parent-instance transform-manager entity-instance)
  (%filament:set-parent
   '(claw-utils:claw-pointer %filament:transform-manager) transform-manager
   '(claw-utils:claw-pointer %filament:transform-manager+instance) entity-instance
   '(claw-utils:claw-pointer %filament:transform-manager+instance) parent-instance))


(defun transform-parent (transform-manager entity-instance)
  (declare (ignore transform-manager entity-instance))
  (error "Not implemented"))
