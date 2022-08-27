(cl:in-package :%alien-works.filament)


;;;
;;; MATERIAL PARSER
;;;
(warp-intricate-builder-option material-builder :package
    %filament:package
  '(claw-utils:claw-pointer %filament::material+builder)
  '(claw-utils:claw-pointer :void)
  '%filament:size-t)


(defmacro with-material-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:material+builder))
         (build-expander (builder)
           `(%filament:build
             '(claw-utils:claw-pointer %filament:material+builder) ,builder
             '(claw-utils:claw-pointer %filament:engine) !::engine)))
    (explode-builder name
                     'material-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-material (engine material)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::material) material))


(defun material-name (material)
  (cffi:foreign-string-to-lisp
   (%filament:get-name
    :const
    '(claw-utils:claw-pointer %filament::material) material)))


(defun default-material-instance (material)
  (%filament:get-default-instance
   '(claw-utils:claw-pointer %filament::material) material))


(defun make-material-instance (material &optional name)
  (%filament:create-instance
   :const
   '(claw-utils:claw-pointer %filament::material) material
   'claw-utils:claw-string (or name (cffi:null-pointer))))


(defun material-instance-name (material)
  (cffi:foreign-string-to-lisp
   (%filament:get-name
    :const
    '(claw-utils:claw-pointer %filament::material-instance) material)))


(defun destroy-material-instance (engine instance)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::material-instance) instance))


(defun (setf material-instance-parameter-float) (value material name)
  (cref:c-with ((fval :float))
    (setf fval (float value 0f0))
    (%filament:set-parameter
     '(claw-utils:claw-pointer %filament::material-instance) material
     'claw-utils:claw-string name
     '(claw-utils:claw-pointer :float) (fval &)))
  value)


(defun (setf material-instance-parameter-float2) (value material name)
  (%filament:set-parameter
   '(claw-utils:claw-pointer %filament::material-instance) material
   'claw-utils:claw-string name
   '(claw-utils:claw-pointer %filament::math+float2) value))


(defun (setf material-instance-parameter-float3) (value material name)
  (%filament:set-parameter
   '(claw-utils:claw-pointer %filament::material-instance) material
   'claw-utils:claw-string name
   '(claw-utils:claw-pointer %filament::math+float3) value))


(defun (setf material-instance-parameter-float4) (value material name)
  (%filament:set-parameter
   '(claw-utils:claw-pointer %filament::material-instance) material
   'claw-utils:claw-string name
   '(claw-utils:claw-pointer %filament::math+float4) value))


(defun (setf material-instance-parameter-mat3) (mat3 material name)
  (%filament:set-parameter
   '(claw-utils:claw-pointer %filament::material-instance) material
   'claw-utils:claw-string name
   '(claw-utils:claw-pointer %filament::math+mat3f) mat3))


(defun (setf material-instance-parameter-mat4) (mat4 material name)
  (%filament:set-parameter
   '(claw-utils:claw-pointer %filament::material-instance) material
   'claw-utils:claw-string name
   '(claw-utils:claw-pointer %filament::math+mat4f) mat4))


(defun (setf material-instance-parameter-sampler) (value material name texture)
  (%filament::set-parameter
   '(claw-utils:claw-pointer %filament::material-instance) material
   'claw-utils:claw-string name
   '(claw-utils:claw-pointer %filament::texture) texture
   '(claw-utils:claw-pointer %filament::texture-sampler) value))
