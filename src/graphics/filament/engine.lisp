(cl:in-package :%alien-works.filament)

(defun create-engine (&optional shared-context)
  (%filament:engine+create
   '%filament::engine+backend (cffi:foreign-enum-value
                               '%filament:engine+backend
                               :opengl)
   '(claw-utils:claw-pointer %filament::engine+platform) (cffi:null-pointer)
   '(claw-utils:claw-pointer :void) (or shared-context (cffi:null-pointer))
   '(claw-utils:claw-pointer %filament::engine+config) (cffi:null-pointer)))


(defun destroy-engine (engine)
  (%filament:engine+destroy
   '(claw-utils:claw-pointer %filament::engine) engine))
