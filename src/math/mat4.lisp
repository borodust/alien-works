(cl:in-package :alien-works.math)


(defun mat4 (mat col row)
  (vec4 (%glm:glm-operator[] '(:pointer %glm:glm-mat4) mat :int col) row))


(defun (setf mat4) (value mat col row)
  (let ((ptr (%glm:glm-operator[] '(:pointer %glm:glm-mat4) mat :int col)))
    (setf (vec4 ptr row) value)))


(defun make-mat4 (x0 x1 x2 x3
                  y0 y1 y2 y3
                  z0 z1 z2 z3
                  w0 w1 w2 w3)
  (iffi:make-intricate-instance
   '%glm:glm-mat4
   :float (float x0 0f0)
   :float (float y0 0f0)
   :float (float z0 0f0)
   :float (float w0 0f0)

   :float (float x1 0f0)
   :float (float y1 0f0)
   :float (float z1 0f0)
   :float (float w1 0f0)

   :float (float x2 0f0)
   :float (float y2 0f0)
   :float (float z2 0f0)
   :float (float w2 0f0)

   :float (float x3 0f0)
   :float (float y3 0f0)
   :float (float z3 0f0)
   :float (float w3 0f0)))


(defun destroy-mat4 (mat)
  (iffi:destroy-intricate-instance '%glm:glm-mat4 mat))


(defmacro with-mat4 ((mat &key
                            (x0 1f0) (x1 0f0) (x2 0f0) (x3 0f0)
                            (y0 0f0) (y1 1f0) (y2 0f0) (y3 0f0)
                            (z0 0f0) (z1 0f0) (z2 1f0) (z3 0f0)
                            (w0 0f0) (w1 0f0) (w2 0f0) (w3 1f0))
                     &body body)
  `(let ((,mat (make-mat4 ,x0 ,x1 ,x2 ,x3
                          ,y0 ,y1 ,y2 ,y3
                          ,z0 ,z1 ,z2 ,z3
                          ,w0 ,w1 ,w2 ,w3)))
     (unwind-protect
          (progn ,@body)
       (destroy-mat4 ,mat))))


(defun rotate-mat4 (result source angle vec3)
  (%glm:glm-rotate
   '(:pointer %glm::glm-mat4) result
   '(:pointer %glm::glm-mat4) source
   ':float (float angle 0f0)
   '(:pointer %glm::glm-vec3) vec3))
