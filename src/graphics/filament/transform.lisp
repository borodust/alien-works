(cl:in-package :%alien-works.graphics)


(defun transform-manager (engine)
  (%filament:filament-get-transform-manager
   '(:pointer %filament::filament-engine) engine))


(defmacro with-transform-instance ((instance entity) transform-manager &body body)
  `(iffi:with-intricate-instance (,instance %filament::filament-transform-manager-instance)
     (%filament:filament-get-instance
      '(:pointer %filament::filament-transform-manager-instance) ,instance
      '(:pointer %filament::filament-transform-manager) ,transform-manager
      '(:pointer %filament::utils-entity) ,entity)
     ,@body))


(defun (setf transform) (mat4f transform-manager entity-instance)
  (%filament:filament-set-transform
   '(:pointer %filament::filament-transform-manager) transform-manager
   '(:pointer %filament::filament-transform-manager-instance) entity-instance
   '(:pointer %filament::filament-math-mat4f) mat4f))
