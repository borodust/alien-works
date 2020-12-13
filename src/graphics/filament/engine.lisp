(cl:in-package :%alien-works.graphics)

(defun create-engine ()
  (%filament:filament-engine-create
   '%filament:filament-engine-backend (cffi:foreign-enum-value
                                       '%filament:filament-engine-backend
                                       :opengl)
   '(:pointer %filament:filament-engine-platform) (cffi:null-pointer)
   '(:pointer :void) (cffi:null-pointer)))


(defun destroy-engine (engine)
  (%filament:filament-engine-destroy
   '(:pointer %filament::filament-engine) engine))
