(cl:in-package :%alien-works.filament)


(defun transform-manager (engine)
  (%filament:get-transform-manager
   '(claw-utils:claw-pointer %filament::engine) engine))


(defmacro with-transform-instance ((instance entity) transform-manager &body body)
  `(iffi:with-intricate-instance (,instance %filament::transform-manager+instance)
     (%filament:get-instance
      :const
      '(claw-utils:claw-pointer %filament::transform-manager+instance) ,instance
      '(claw-utils:claw-pointer %filament::transform-manager) ,transform-manager
      '(claw-utils:claw-pointer %filament::utils+entity) ,entity)
     ,@body))


(defun (setf transform) (mat4f transform-manager entity-instance)
  (%filament:set-transform
   '(claw-utils:claw-pointer %filament::transform-manager) transform-manager
   '(claw-utils:claw-pointer %filament::transform-manager+instance) entity-instance
   '(claw-utils:claw-pointer %filament::math+mat4f) mat4f))
