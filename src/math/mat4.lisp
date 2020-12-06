(cl:in-package :alien-works.math)


(defun mat4 (mat col row)
  (vec4 (%glm:glm-operator[] '(:pointer %glm:glm-mat4) mat :int col) row))


(defun (setf mat4) (value mat col row)
  (let ((ptr (%glm:glm-operator[] '(:pointer %glm:glm-mat4) mat :int col)))
    (setf (vec4 ptr row) value)))


(defun create-mat4 (x0 x1 x2 x3
                    y0 y1 y2 y3
                    z0 z1 z2 z3
                    w0 w1 w2 w3)
  (let ((mat (iffi:make-intricate-instance '%glm:glm-mat4)))
    (setf (mat4 mat 0 0) x0
          (mat4 mat 1 0) y0
          (mat4 mat 2 0) z0
          (mat4 mat 3 0) w0

          (mat4 mat 0 1) x1
          (mat4 mat 1 1) y1
          (mat4 mat 2 1) z1
          (mat4 mat 3 1) w1

          (mat4 mat 0 2) x2
          (mat4 mat 1 2) y2
          (mat4 mat 2 2) z2
          (mat4 mat 3 2) w2

          (mat4 mat 0 3) x3
          (mat4 mat 1 3) y3
          (mat4 mat 2 3) z3
          (mat4 mat 3 3) w3)
    mat))


(defun destroy-mat4 (mat)
  (iffi:destroy-intricate-instance '%glm:glm-mat4 mat))


(defmacro with-mat4 ((mat &key
                            (x0 1f0) (x1 0f0) (x2 0f0) (x3 0f0)
                            (y0 0f0) (y1 1f0) (y2 0f0) (y3 0f0)
                            (z0 0f0) (z1 0f0) (z2 1f0) (z3 0f0)
                            (w0 0f0) (w1 0f0) (w2 0f0) (w3 1f0))
                     &body body)
  `(let ((,mat (create-mat4 ,x0 ,x1 ,x2 ,x3
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
