(cl:in-package :alien-works.math)


(defun vec4 (vec idx)
  (cffi:mem-ref (%glm:glm-operator[] '(:pointer %glm:glm-vec4) vec :int idx) :float))


(defun (setf vec4) (value vec idx)
  (let ((ptr (%glm:glm-operator[] '(:pointer %glm:glm-vec4) vec :int idx)))
    (setf (cffi:mem-ref ptr :float) (float value 0f0))))


(defun make-vec4 (x y z w)
  (let ((instance (iffi:make-intricate-instance '%glm:glm-vec4)))
    (setf (vec4 instance 0) x
          (vec4 instance 1) y
          (vec4 instance 2) z
          (vec4 instance 3) w)
    instance))


(defun destroy-vec4 (vec)
  (iffi:destroy-intricate-instance '%glm:glm-vec4 vec))


(defun vec4-add (result this that)
  (%glm:glm-operator+
   '(:pointer %glm:glm-vec4) result
   '(:pointer %glm:glm-vec4) this
   '(:pointer %glm:glm-vec4) that))


(defun vec4-mult (result this that)
  (%glm:glm-operator*
   '(:pointer %glm:glm-vec4) result
   '(:pointer %glm:glm-vec4) this
   '(:pointer %glm:glm-vec4) that))


(defun vec4-dot (this that)
  (%glm:glm-dot
   '(:pointer %glm:glm-vec4) this
   '(:pointer %glm:glm-vec4) that))
