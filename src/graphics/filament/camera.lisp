(cl:in-package :%alien-works.filament)


(defun create-camera (engine entity)
  (%filament:create-camera
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::utils+entity) entity))


(defun destroy-camera (engine camera)
  (%filament:destroy-camera-component
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::utils+entity) camera))


(u:define-enumval-extractor projection-enum %filament:camera+projection)


(defun update-camera-projection (camera kind left right bottom top near far)
  (%filament:set-projection
   '(claw-utils:claw-pointer %filament::camera) camera
   '%filament::camera+projection kind
   :double (float left 0d0)
   :double (float right 0d0)
   :double (float bottom 0d0)
   :double (float top 0d0)
   :double (float near 0d0)
   :double (float far 0d0)))


(defun update-camera-fov-projection (camera fov aspect near far direction)
  (%filament::set-projection
   '(claw-utils:claw-pointer %filament::camera) camera
   :double (float fov 0d0)
   :double (float aspect 0d0)
   :double (float near 0d0)
   :double (float far 0d0)
   '%filament::camera+fov direction))


(defun update-camera-lens-projection (camera focal-length aspect near far)
  (%filament::set-lens-projection
   '(claw-utils:claw-pointer %filament::camera) camera
   :double (float focal-length 0d0)
   :double (float aspect 0d0)
   :double (float near 0d0)
   :double (float far 0d0)))


(defun update-camera-model-matrix (camera transform)
  (%filament:set-model-matrix
   '(claw-utils:claw-pointer %filament::camera) camera
   '(claw-utils:claw-pointer %filament::math+mat4f) transform))
