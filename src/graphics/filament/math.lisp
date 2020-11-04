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
  (setf (cffi:mem-ref (%filament::filament-math-details-operator[]
                       '(:pointer %filament::filament-math-details-t-vec4<float>) vec
                       '%filament::size-t idx)
                      :float)
        (float value 0f0)))


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
  (setf (cffi:mem-ref (%filament::filament-math-details-operator[]
                       '(:pointer %filament::filament-math-details-t-vec3<float>) vec
                       '%filament::size-t idx)
                      :float)
        (float value 0f0)))

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
