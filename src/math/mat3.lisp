(cl:in-package :alien-works.math)


(defun mat3 (mat col row)
  (vec4 (%glm:glm-operator[] '(:pointer %glm:glm-mat3) mat :int col) row))


(defun (setf mat3) (value mat col row)
  (let ((ptr (%glm:glm-operator[] '(:pointer %glm:glm-mat3) mat :int col)))
    (setf (vec4 ptr row) value)))


(defun make-mat3 (x0 x1 x2
                  y0 y1 y2
                  z0 z1 z2)
  (iffi:make-intricate-instance
   '%glm:glm-mat3
   :float (float x0 0f0)
   :float (float y0 0f0)
   :float (float z0 0f0)

   :float (float x1 0f0)
   :float (float y1 0f0)
   :float (float z1 0f0)

   :float (float x2 0f0)
   :float (float y2 0f0)
   :float (float z2 0f0)))


(defun make-mat3-from-basis (x-vec3 y-vec3 z-vec3)
  (iffi:make-intricate-instance
   '%glm:glm-mat3
   '(:pointer %glm::glm-vec3) x-vec3
   '(:pointer %glm::glm-vec3) y-vec3
   '(:pointer %glm::glm-vec3) z-vec3))


(defun destroy-mat3 (mat)
  (iffi:destroy-intricate-instance '%glm:glm-mat3 mat))


(defmacro with-mat3 ((mat &key
                            (x0 1f0) (x1 0f0) (x2 0f0)
                            (y0 0f0) (y1 1f0) (y2 0f0)
                            (z0 0f0) (z1 0f0) (z2 1f0))
                     &body body)
  `(let ((,mat (make-mat3 ,x0 ,x1 ,x2
                          ,y0 ,y1 ,y2
                          ,z0 ,z1 ,z2)))
     (unwind-protect
          (progn ,@body)
       (destroy-mat3 ,mat))))


(defmacro with-mat3-from-basis ((mat x-vec y-vec z-vec) &body body)
  `(let ((,mat (make-mat3-from-basis ,x-vec ,y-vec ,z-vec)))
     (unwind-protect
          (progn ,@body)
       (destroy-mat3 ,mat))))
