(cl:in-package :%alien-works.filament)

(defun create-engine (&optional shared-context)
  (%filament:filament+engine+create
   '%filament:filament+engine+backend (cffi:foreign-enum-value
                                       '%filament:filament+engine+backend
                                       :opengl)
   '(:pointer %filament:filament+engine+platform) (cffi:null-pointer)
   '(:pointer :void) (or shared-context (cffi:null-pointer))))


(defun destroy-engine (engine)
  (%filament:filament+engine+destroy
   '(:pointer %filament::filament+engine) engine))
