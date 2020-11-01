(cl:in-package :alien-works.graphics.filament)


(defun create-camera (engine)
  (%filament:filament-create-camera
   '(:pointer %filament::filament-engine) engine))


(defun destroy-camera (engine camera)
  (%filament:filament-destroy
   '(:pointer %filament::filament-engine) engine
   '(:pointer %filament::filament-camera) camera))
