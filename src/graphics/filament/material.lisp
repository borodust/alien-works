(cl:in-package :%alien-works.filament)


;;;
;;; MATERIAL PARSER
;;;
(warp-intricate-builder-option material-builder :package
    %filament:filament+package
  '(:pointer %filament::filament+material+builder)
  '(:pointer :void)
  '%filament:size-t)


(defmacro with-material-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament+material+builder))
         (build-expander (builder)
           `(%filament:filament+build
             '(:pointer %filament:filament+material+builder) ,builder
             '(:pointer %filament:filament+engine) !::engine)))
    (explode-builder name
                     'material-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun default-material-instance (material)
  (%filament:filament+get-default-instance
   '(:pointer %filament::filament+material) material))


(defun make-material-instance (material &optional name)
  (%filament:filament+create-instance
   :const
   '(:pointer %filament::filament+material) material
   'claw-utils:claw-string (or name (cffi:null-pointer))))


(defun destroy-material-instance (engine instance)
  (%filament:filament+destroy
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::filament+material-instance) instance))


(defun (setf material-instance-parameter-float) (value material name)
  (%filament:filament+set-parameter
   '(:pointer %filament::filament+material-instance) material
   'claw-utils:claw-string name
   :float value))


(defun (setf material-instance-parameter-float2) (value material name)
  (%filament:filament+set-parameter
   '(:pointer %filament::filament+material-instance) material
   'claw-utils:claw-string name
   '(:pointer %filament::filament+math+float2) value))


(defun (setf material-instance-parameter-float3) (value material name)
  (%filament:filament+set-parameter
   '(:pointer %filament::filament+material-instance) material
   'claw-utils:claw-string name
   '(:pointer %filament::filament+math+float3) value))


(defun (setf material-instance-parameter-float4) (value material name)
  (%filament:filament+set-parameter
   '(:pointer %filament::filament+material-instance) material
   'claw-utils:claw-string name
   '(:pointer %filament::filament+math+float4) value))


(defun (setf material-instance-parameter-mat3) (mat3 material name)
  (%filament:filament+set-parameter
   '(:pointer %filament::filament+material-instance) material
   'claw-utils:claw-string name
   '(:pointer %filament::filament+math+mat3f) mat3))


(defun (setf material-instance-parameter-mat4) (mat4 material name)
  (%filament:filament+set-parameter
   '(:pointer %filament::filament+material-instance) material
   'claw-utils:claw-string name
   '(:pointer %filament::filament+math+mat4f) mat4))


(defun (setf material-instance-parameter-sampler) (value material name texture)
  (%filament::filament+set-parameter
   '(:pointer %filament::filament+material-instance) material
   'claw-utils:claw-string name
   '(:pointer %filament::filament+texture) texture
   '(:pointer %filament::filament+texture-sampler) value))
