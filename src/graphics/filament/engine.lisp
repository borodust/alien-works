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


;;;
;;; SKYBOX
;;;
(defun expand-skybox-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:environment
       (%explode-function
        '(%filament:filament-environment
          '(:pointer %filament:filament-skybox-builder)
          '(:pointer %filament:filament-texture))))
      (:show-sun
       (%explode-function
        '(%filament:filament-show-sun
          '(:pointer %filament:filament-skybox-builder)
          ':bool)))
      (:intensity
       (%explode-function
        '(%filament:filament-intensity
          '(:pointer %filament:filament-skybox-builder)
          ':float)))
      (:color
       (%explode-function
        '(%filament:filament-color
          '(:pointer %filament:filament-skybox-builder)
          '(:pointer %filament:filament-math-float4)))))))


(defmacro with-skybox-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament-skybox-builder))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-skybox-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine)))
    (explode-builder name
                     #'expand-skybox-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))
