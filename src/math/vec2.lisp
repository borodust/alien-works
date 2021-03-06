(cl:in-package :alien-works.math)


(defun vec2 (vec idx)
  (cffi:mem-ref (%glm:glm+operator[] '(:pointer %glm:glm+vec2) vec :int idx) :float))


(defun (setf vec2) (value vec idx)
  (let ((ptr (%glm:glm+operator[] '(:pointer %glm:glm+vec2) vec :int idx)))
    (setf (cffi:mem-ref ptr :float) (float value 0f0))))


(defun make-vec2 (x y)
  (iffi:make-intricate-instance '%glm:glm+vec2
                                :float (float x 0f0)
                                :float (float y 0f0)))


(defun destroy-vec2 (vec)
  (iffi:destroy-intricate-instance '%glm:glm+vec2 vec))


(defun vec2-add (result this that)
  (%glm:glm+operator+
   '(:pointer %glm:glm+vec2) result
   '(:pointer %glm:glm+vec2) this
   '(:pointer %glm:glm+vec2) that))


(defun vec2-mult (result this that)
  (%glm:glm+operator*
   '(:pointer %glm:glm+vec2) result
   '(:pointer %glm:glm+vec2) this
   '(:pointer %glm:glm+vec2) that))
