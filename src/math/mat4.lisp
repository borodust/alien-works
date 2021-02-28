(cl:in-package :alien-works.math)


(u:definline mat4 (mat col row)
  (vec4 (%glm:glm+operator[] '(:pointer %glm:glm+mat4) mat :int col) row))


(u:definline (setf mat4) (value mat col row)
  (let ((ptr (%glm:glm+operator[] '(:pointer %glm:glm+mat4) mat :int col)))
    (setf (vec4 ptr row) value)))


(u:definline make-mat4 (x0 x1 x2 x3
                        y0 y1 y2 y3
                        z0 z1 z2 z3
                        w0 w1 w2 w3)
  (let ((instance (iffi:make-intricate-instance '%glm:glm+mat4)))
    (setf (mat4 instance 0 0) x0
          (mat4 instance 0 1) y0
          (mat4 instance 0 2) z0
          (mat4 instance 0 3) w0

          (mat4 instance 1 0) x1
          (mat4 instance 1 1) y1
          (mat4 instance 1 2) z1
          (mat4 instance 1 3) w1

          (mat4 instance 2 0) x2
          (mat4 instance 2 1) y2
          (mat4 instance 2 2) z2
          (mat4 instance 2 3) w2

          (mat4 instance 3 0) x3
          (mat4 instance 3 1) y3
          (mat4 instance 3 2) z3
          (mat4 instance 3 3) w3)
    instance))


(u:definline destroy-mat4 (mat)
  (iffi:destroy-intricate-instance '%glm:glm+mat4 mat))


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


(defmacro with-mat4* ((&rest declarations) &body body)
  (u:expand-multibinding 'with-mat4 declarations body))


(u:definline rotate-mat4 (result source angle vec3)
  (%glm:glm+rotate
   '(:pointer %glm::glm+mat4) result
   '(:pointer %glm::glm+mat4) source
   ':float (float angle 0f0)
   '(:pointer %glm::glm+vec3) vec3))


(u:definline translate-mat4 (result source vec3)
  (%glm:glm+translate
   '(:pointer %glm::glm+mat4) result
   '(:pointer %glm::glm+mat4) source
   '(:pointer %glm::glm+vec3) vec3))


(u:definline scale-mat4 (result source vec3)
  (%glm:glm+scale
   '(:pointer %glm::glm+mat4) result
   '(:pointer %glm::glm+mat4) source
   '(:pointer %glm::glm+vec3) vec3))


(u:definline mat4-mult (result this that)
  (%glm:glm+operator*
   '(:pointer %glm::glm+mat4) result
   '(:pointer %glm::glm+mat4) this
   '(:pointer %glm::glm+mat4) that))
