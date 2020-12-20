(cl:in-package :alien-works.physics.physx)


(defmacro with-scale ((scale) &body body)
  `(iffi:with-intricate-instance (,scale %physx:physx+px-tolerances-scale)
     (progn ,@body)))


(defun make-vec3 ()
  (iffi:make-intricate-instance '%physx:physx+px-vec3 :float 0f0))


(defun destroy-vec3 (vec)
  (iffi:destroy-intricate-instance '%physx:physx+px-vec3 vec))


(defmacro with-vec3 ((vec) &body body)
  `(let ((,vec (make-vec3)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec3 ,vec))))


(defun x (vec)
  (iffi:intricate-slot-value vec
                             '%physx:physx+px-vec3
                             '%physx:x))


(defun (setf x) (value vec)
  (setf (iffi:intricate-slot-value vec
                                   '%physx:physx+px-vec3
                                   '%physx:x)
        (float value 0f0)))


(defun y (vec)
  (iffi:intricate-slot-value vec
                             '%physx:physx+px-vec3
                             '%physx:y))


(defun (setf y) (value vec)
  (setf (iffi:intricate-slot-value vec
                                   '%physx:physx+px-vec3
                                   '%physx:y)
        (float value 0f0)))


(defun z (vec)
  (iffi:intricate-slot-value vec
                             '%physx:physx+px-vec3
                             '%physx:z))


(defun (setf z) (value vec)
  (setf (iffi:intricate-slot-value vec
                                   '%physx:physx+px-vec3
                                   '%physx:z)
        (float value 0f0)))


(defun make-transform ()
  (iffi:make-intricate-instance '%physx:physx+px-transform
                                '%physx:physx+px-identity :px-identity))


(defun destroy-transform (transform)
  (iffi:destroy-intricate-instance '%physx:physx+px-transform transform))


(defmacro with-transform ((transform) &body body)
  `(let ((,transform (make-transform)))
     (unwind-protect
          (progn ,@body)
       (destroy-transform ,transform))))


(defun transform-position (transform)
  (iffi:intricate-slot-value transform '%physx:physx+px-transform '%physx:p))


(defun transform-rotation (transform)
  (iffi:intricate-slot-value transform '%physx:physx+px-transform '%physx:q))
