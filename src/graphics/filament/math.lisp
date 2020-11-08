(cl:in-package :%alien-works.graphics)

;;;
;;; VEC4
;;;
(defun vec4f (vec idx)
  (cffi:mem-ref (%filament::filament-math-details-operator[]
                 '(:pointer %filament::filament-math-details-t-vec4<float>) vec
                 '%filament::size-t idx)
                :float))


(defun (setf vec4f) (value vec idx)
  (let ((mem (%filament::filament-math-details-operator[]
              '(:pointer %filament::filament-math-details-t-vec4<float>) vec
              '%filament::size-t idx)))
    (setf (cffi:mem-ref mem :float)
          (float value 0f0))))


(defun create-vec4f (x y z w)
  (let ((vec (iffi:make-intricate-instance '%filament:filament-math-details-t-vec4<float>)))
    (setf (vec4f vec 0) x
          (vec4f vec 1) y
          (vec4f vec 2) z
          (vec4f vec 3) w)
    vec))


(defun destroy-vec4f (vec)
  (iffi:destroy-intricate-instance '%filament:filament-math-details-t-vec4<float> vec))


(defmacro with-vec4f ((vec &optional (x 0f0) (y 0f0) (z 0f0) (w 1f0)) &body body)
  `(let ((,vec (create-vec4f ,x ,y ,z ,w)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec4f ,vec))))


;;;
;;; VEC3
;;;
(defun vec3f (vec idx)
  (cffi:mem-ref (%filament::filament-math-details-operator[]
                 '(:pointer %filament::filament-math-details-t-vec3<float>) vec
                 '%filament::size-t idx)
                :float))


(defun (setf vec3f) (value vec idx)
  (let ((mem (%filament::filament-math-details-operator[]
              '(:pointer %filament::filament-math-details-t-vec3<float>) vec
              '%filament::size-t idx)))
    (setf (cffi:mem-ref mem :float) (float value 0f0))))


(defun create-vec3f (x y z)
  (let ((vec (iffi:make-intricate-instance '%filament:filament-math-details-t-vec3<float>)))
    (setf (vec3f vec 0) x
          (vec3f vec 1) y
          (vec3f vec 2) z)
    vec))


(defun destroy-vec3f (vec)
  (iffi:destroy-intricate-instance '%filament:filament-math-details-t-vec3<float> vec))


(defmacro with-vec3f ((vec &optional (x 0f0) (y 0f0) (z 0f0)) &body body)
  `(let ((,vec (create-vec3f ,x ,y ,z)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec3f ,vec))))

;;;
;;; MAT4
;;;
(defun mat4f (mat row col)
  (let ((column (%filament::filament-math-details-operator[]
                 '(:pointer %filament::filament-math-details-t-mat44<float>) mat
                 '%filament::size-t col)))
    (vec4f column row)))


(defun (setf mat4f) (value mat row col)
  (let ((column (%filament::filament-math-details-operator[]
                 '(:pointer %filament::filament-math-details-t-mat44<float>) mat
                 '%filament::size-t col)))
    (setf (vec4f column row) value)))


(defun create-mat4f (ax ay az aw
                     bx by bz bw
                     cx cy cz cw
                     dx dy dz dw)
  (let ((mat (iffi:make-intricate-instance '%filament:filament-math-mat4f)))
    (setf (mat4f mat 0 0) ax
          (mat4f mat 0 1) ay
          (mat4f mat 0 2) az
          (mat4f mat 0 3) aw

          (mat4f mat 1 0) bx
          (mat4f mat 1 1) by
          (mat4f mat 1 2) bz
          (mat4f mat 1 3) bw

          (mat4f mat 2 0) cx
          (mat4f mat 2 1) cy
          (mat4f mat 2 2) cz
          (mat4f mat 2 3) cw

          (mat4f mat 3 0) dx
          (mat4f mat 3 1) dy
          (mat4f mat 3 2) dz
          (mat4f mat 3 3) dw)
    mat))


(defun create-identity-mat4f ()
  (iffi:make-intricate-instance '%filament:filament-math-mat4f))


(defun destroy-mat4f (mat)
  (iffi:destroy-intricate-instance '%filament:filament-math-mat4f mat))


(defmacro with-mat4f ((mat &optional
                             (ax 1f0) (ay 0f0) (az 0f0) (aw 0f0)
                             (bx 0f0) (by 1f0) (bz 0f0) (bw 0f0)
                             (cx 0f0) (cy 0f0) (cz 1f0) (cw 0f0)
                             (dx 0f0) (dy 0f0) (dz 0f0) (dw 1f0))
                      &body body)
  `(let ((,mat (create-mat4f ,ax ,ay ,az ,aw
                             ,bx ,by ,bz ,bw
                             ,cx ,cy ,cz ,cw
                             ,dx ,dy ,dz ,dw)))
     (unwind-protect
          (progn ,@body)
       (destroy-mat4f ,mat))))
