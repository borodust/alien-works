(cl:in-package :alien-works.physics.physx)


(defun make-scene-descriptor (physics dispatcher)
  (let* ((scale (%physx:physx+get-tolerances-scale '(:pointer %physx:physx+px-physics) physics))
         (descriptor (iffi:make-intricate-instance '%physx:physx+px-scene-desc
                                                   '(:pointer %physx:physx+px-tolerances-scale) scale))
         (default-filter (iffi:intricate-function-pointer
                          '%physx:physx+px-default-simulation-filter-shader
                          '(:pointer %physx:physx+px-filter-flags)
                          '%physx:physx+px-filter-object-attributes
                          '(:pointer %physx:physx+px-filter-data)
                          '%physx:physx+px-filter-object-attributes
                          '(:pointer %physx:physx+px-filter-data)
                          '(:pointer %physx:physx+px-pair-flags)
                          '(:pointer :void)
                          '%physx:physx+px-u32)))
    (setf
     (iffi:intricate-slot-value descriptor '%physx:physx+px-scene-desc '%physx:cpu-dispatcher) dispatcher
     (iffi:intricate-slot-value descriptor '%physx:physx+px-scene-desc '%physx:filter-shader) default-filter)
    descriptor))


(defun scene-descriptor-valid-p (descriptor)
  (%physx:physx+is-valid '(:pointer %physx:physx+px-scene-desc) descriptor))


(defun destroy-scene-descriptor (descriptor)
  (iffi:destroy-intricate-instance '%physx:physx+px-scene-desc descriptor))


(defun (setf gravity) (value descriptor)
  (setf (iffi:intricate-slot-value descriptor '%physx:physx+px-scene-desc '%physx:gravity) value))


(defmacro with-scene-descriptor ((descriptor physics dispatcher) &body body)
  `(let ((,descriptor (make-scene-descriptor ,physics ,dispatcher)))
     (unwind-protect
          (progn ,@body)
       (destroy-scene-descriptor ,descriptor))))


(defun make-scene (physics descriptor)
  (%physx:physx+create-scene
   '(:pointer %physx:physx+px-physics) physics
   '(:pointer %physx:physx+px-scene-desc) descriptor))


(defun destroy-scene (scene)
  (%physx:physx+release '(:pointer %physx:physx+px-scene) scene))


(defun simulate-scene (scene step)
  (%physx:physx+simulate
   '(:pointer %physx:physx+px-scene) scene
   '%physx:physx+px-real (float step 0f0)
   '(:pointer %physx:physx+px-base-task) (cffi:null-pointer)
   '(:pointer :void) (cffi:null-pointer)
   '%physx:physx+px-u32 0
   ':bool t))


(defun finish-simulation (scene)
  (%physx:physx+fetch-results
   '(:pointer %physx:physx+px-scene) scene
   ':bool t
   '(:pointer %physx:physx+px-u32) (cffi:null-pointer)))
