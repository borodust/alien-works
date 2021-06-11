(cl:in-package :%alien-works.filament)


(defun create-camera (engine entity)
  (%filament:filament+create-camera
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::utils+entity) entity))


(defun destroy-camera (engine camera)
  (%filament:filament+destroy-camera-component
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::utils+entity) camera))


(u:define-enumval-extractor projection-enum %filament:filament+camera+projection)


(defun update-camera-projection (camera kind left right bottom top near far)
  (%filament:filament+set-projection
   '(:pointer %filament::filament+camera) camera
   '%filament::filament+camera+projection kind
   :double (float left 0d0)
   :double (float right 0d0)
   :double (float bottom 0d0)
   :double (float top 0d0)
   :double (float near 0d0)
   :double (float far 0d0)))


(defun update-camera-fov-projection (camera fov aspect near far direction)
  (%filament::filament+set-projection
   '(:pointer %filament::filament+camera) camera
   :double (float fov 0d0)
   :double (float aspect 0d0)
   :double (float near 0d0)
   :double (float far 0d0)
   '%filament::filament+camera+fov direction))


(defun update-camera-lens-projection (camera focal-length aspect near far)
  (%filament::filament+set-lens-projection
   '(:pointer %filament::filament+camera) camera
   :double (float focal-length 0d0)
   :double (float aspect 0d0)
   :double (float near 0d0)
   :double (float far 0d0)))


(defun update-camera-model-matrix (camera transform)
  (%filament:filament+set-model-matrix
   '(:pointer %filament::filament+camera) camera
   '(:pointer %filament::filament+math+mat4f) transform))
