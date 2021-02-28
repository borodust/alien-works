(cl:in-package :alien-works.math)


(u:definline vec4 (vec idx)
  (cffi:mem-ref (%glm:glm+operator[] '(:pointer %glm:glm+vec4) vec :int idx) :float))


(u:definline (setf vec4) (value vec idx)
  (let ((ptr (%glm:glm+operator[] '(:pointer %glm:glm+vec4) vec :int idx)))
    (setf (cffi:mem-ref ptr :float) (float value 0f0))))

(u:definline make-vec4 (x y z w)
  (iffi:make-intricate-instance '%glm:glm+vec4
                                :float (float x 0f0)
                                :float (float y 0f0)
                                :float (float z 0f0)
                                :float (float w 0f0)))

(u:definline destroy-vec4 (vec)
  (iffi:destroy-intricate-instance '%glm:glm+vec4 vec))


(u:definline vec4-add (result this that)
  (%glm:glm+operator+
   '(:pointer %glm:glm+vec4) result
   '(:pointer %glm:glm+vec4) this
   '(:pointer %glm:glm+vec4) that))


(u:definline vec4-mult (result this that)
  (%glm:glm+operator*
   '(:pointer %glm:glm+vec4) result
   '(:pointer %glm:glm+vec4) this
   '(:pointer %glm:glm+vec4) that))


(u:definline vec4-dot (this that)
  (%glm:glm+dot
   '(:pointer %glm:glm+vec4) this
   '(:pointer %glm:glm+vec4) that))
