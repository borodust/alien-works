(cl:in-package :alien-works.math)


(u:definline vec2 (vec idx)
  (cffi:mem-ref (%glm:glm+operator[] '(:pointer %glm:glm+vec2) vec :int idx) :float))


(u:definline (setf vec2) (value vec idx)
  (let ((ptr (%glm:glm+operator[] '(:pointer %glm:glm+vec2) vec :int idx)))
    (setf (cffi:mem-ref ptr :float) (float value 0f0))))


(u:definline %math:vec2-element-ptr (vec idx)
  (%glm:glm+operator[] '(:pointer %glm:glm+vec2) vec :int idx))


(u:definline %math:vec2-ptr (vec)
  (%glm:glm+value-ptr '(:pointer %glm:glm+vec2) vec))


(defmacro with-vec2 ((vec &key (x 0f0) (y 0f0)) &body body)
  `(let ((,vec (make-vec2 ,x ,y)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec2 ,vec))))


(defmacro with-vec2* ((&rest declarations) &body body)
  (u:expand-multibinding 'with-vec2 declarations body))


(defun make-vec2 (x y)
  (iffi:make-intricate-instance '%glm:glm+vec2
                                :float (float x 0f0)
                                :float (float y 0f0)))


(defun destroy-vec2 (vec)
  (iffi:destroy-intricate-instance '%glm:glm+vec2 vec))


(u:definline vec2-add (result this that)
  (%glm:glm+operator+
   '(:pointer %glm:glm+vec2) result
   '(:pointer %glm:glm+vec2) this
   '(:pointer %glm:glm+vec2) that))


(u:definline vec2-subt (result this that)
  (%glm:glm+operator-
   '(:pointer %glm:glm+vec2) result
   '(:pointer %glm:glm+vec2) this
   '(:pointer %glm:glm+vec2) that))


(u:definline vec2-mult (result this that)
  (%glm:glm+operator*
   '(:pointer %glm:glm+vec2) result
   '(:pointer %glm:glm+vec2) this
   '(:pointer %glm:glm+vec2) that))


(u:definline vec2-scalar-mult (result vec2 scalar)
  (%glm:glm+operator*
   '(:pointer %glm:glm+vec2) result
   '(:pointer %glm:glm+vec2) vec2
   :float (float scalar 0f0)))


(u:definline vec2-dot (this that)
  (%glm:glm+dot
   '(:pointer %glm:glm+vec2) this
   '(:pointer %glm:glm+vec2) that))


(u:definline vec2-normalize (result vec2)
  (%glm:glm+normalize
   '(:pointer %glm:glm+vec2) result
   '(:pointer %glm:glm+vec2) vec2))


(u:definline vec2-copy (result vec2)
  (%glm:glm+operator=
   '(:pointer %glm:glm+vec2) result
   '(:pointer %glm:glm+vec2) vec2))


(u:definline vec2-equal (this that &optional (epsilon +epsilon+))
  (with-vec2 (result)
    (vec2-subt result this that)
    (and (>= epsilon (abs (vec2 result 0)))
         (>= epsilon (abs (vec2 result 1))))))


(u:definline vec2-length (vec2)
  (%glm:glm+length
   '(:pointer %glm:glm+vec2) vec2))
