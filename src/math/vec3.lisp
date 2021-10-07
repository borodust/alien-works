(cl:in-package :alien-works.math)


(u:definline vec3 (vec idx)
  (cffi:mem-ref (%glm:glm+operator[] '(:pointer %glm:glm+vec3) vec :int idx) :float))


(u:definline %math:vec3-element-ptr (vec idx)
  (%glm:glm+operator[] '(:pointer %glm:glm+vec3) vec :int idx))


(u:definline %math:vec3-ptr (vec)
  (%glm:glm+value-ptr '(:pointer %glm:glm+vec3) vec))


(u:definline (setf vec3) (value vec idx)
  (let ((ptr (%glm:glm+operator[] '(:pointer %glm:glm+vec3) vec :int idx)))
    (setf (cffi:mem-ref ptr :float) (float value 0f0))))


(u:definline make-vec3 (x y z)
  (iffi:make-intricate-instance '%glm:glm+vec3
                                :float (float x 0f0)
                                :float (float y 0f0)
                                :float (float z 0f0)))

(u:definline destroy-vec3 (vec)
  (iffi:destroy-intricate-instance '%glm:glm+vec3 vec))


(defmacro with-vec3 ((vec &key (x 0f0) (y 0f0) (z 0f0)) &body body)
  `(let ((,vec (make-vec3 ,x ,y ,z)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec3 ,vec))))


(defmacro with-vec3* ((&rest declarations) &body body)
  (u:expand-multibinding 'with-vec3 declarations body))


(u:definline vec3-add (result this that)
  (%glm:glm+operator+
   '(:pointer %glm:glm+vec3) result
   '(:pointer %glm:glm+vec3) this
   '(:pointer %glm:glm+vec3) that))


(u:definline vec3-mult (result this that)
  (%glm:glm+operator*
   '(:pointer %glm:glm+vec3) result
   '(:pointer %glm:glm+vec3) this
   '(:pointer %glm:glm+vec3) that))


(u:definline vec3-dot (this that)
  (%glm:glm+dot
   '(:pointer %glm:glm+vec3) this
   '(:pointer %glm:glm+vec3) that))


(u:definline vec3-cross (result this that)
  (%glm:glm+cross
   '(:pointer %glm:glm+vec3) result
   '(:pointer %glm:glm+vec3) this
   '(:pointer %glm:glm+vec3) that))
