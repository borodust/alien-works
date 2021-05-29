(cl:in-package :%alien-works.filament)

;;;
;;; VEC4
;;;
(defun vec4f (vec idx)
  (cffi:mem-ref (%filament::filament+math+details+operator[]
                 '(:pointer %filament::filament+math+details+t-vec4<float>) vec
                 '%filament::size-t idx)
                :float))


(defun (setf vec4f) (value vec idx)
  (let ((mem (%filament::filament+math+details+operator[]
              '(:pointer %filament::filament+math+details+t-vec4<float>) vec
              '%filament::size-t idx)))
    (setf (cffi:mem-ref mem :float)
          (float value 0f0))))


(defun create-vec4f (x y z w)
  (let ((vec (iffi:make-intricate-instance '%filament:filament+math+details+t-vec4<float>)))
    (setf (vec4f vec 0) x
          (vec4f vec 1) y
          (vec4f vec 2) z
          (vec4f vec 3) w)
    vec))


(defun destroy-vec4f (vec)
  (iffi:destroy-intricate-instance '%filament:filament+math+details+t-vec4<float> vec))


(defmacro with-vec4f ((vec &optional (x 0f0) (y 0f0) (z 0f0) (w 1f0)) &body body)
  `(let ((,vec (create-vec4f ,x ,y ,z ,w)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec4f ,vec))))


;;;
;;; VEC3
;;;
(defun vec3f (vec idx)
  (cffi:mem-ref (%filament::filament+math+details+operator[]
                 '(:pointer %filament::filament+math+details+t-vec3<float>) vec
                 '%filament::size-t idx)
                :float))


(defun (setf vec3f) (value vec idx)
  (let ((mem (%filament::filament+math+details+operator[]
              '(:pointer %filament::filament+math+details+t-vec3<float>) vec
              '%filament::size-t idx)))
    (setf (cffi:mem-ref mem :float) (float value 0f0))))


(defun create-vec3f (x y z)
  (let ((vec (iffi:make-intricate-instance '%filament:filament+math+details+t-vec3<float>)))
    (setf (vec3f vec 0) x
          (vec3f vec 1) y
          (vec3f vec 2) z)
    vec))


(defun destroy-vec3f (vec)
  (iffi:destroy-intricate-instance '%filament:filament+math+details+t-vec3<float> vec))


(defmacro with-vec3f ((vec &optional (x 0f0) (y 0f0) (z 0f0)) &body body)
  `(let ((,vec (create-vec3f ,x ,y ,z)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec3f ,vec))))


;;;
;;; VEC2
;;;
(defun vec2f (vec idx)
  (cffi:mem-ref (%filament::filament+math+details+operator[]
                 '(:pointer %filament::filament+math+details+t-vec2<float>) vec
                 '%filament::size-t idx)
                :float))


(defun (setf vec2f) (value vec idx)
  (let ((mem (%filament::filament+math+details+operator[]
              '(:pointer %filament::filament+math+details+t-vec2<float>) vec
              '%filament::size-t idx)))
    (setf (cffi:mem-ref mem :float) (float value 0f0))))


(defun create-vec2f (x y)
  (let ((vec (iffi:make-intricate-instance '%filament:filament+math+details+t-vec2<float>)))
    (setf (vec2f vec 0) x
          (vec2f vec 1) y)
    vec))


(defun destroy-vec2f (vec)
  (iffi:destroy-intricate-instance '%filament:filament+math+details+t-vec2<float> vec))


(defmacro with-vec2f ((vec &optional (x 0f0) (y 0f0)) &body body)
  `(let ((,vec (create-vec2f ,x ,y)))
     (unwind-protect
          (progn ,@body)
       (destroy-vec2f ,vec))))

;;;
;;; MAT4
;;;
(defun mat4f (mat row col)
  (let ((column (%filament::filament+math+details+operator[]
                 '(:pointer %filament::filament+math+details+t-mat44<float>) mat
                 '%filament::size-t col)))
    (vec4f column row)))


(defun (setf mat4f) (value mat row col)
  (let ((column (%filament::filament+math+details+operator[]
                 '(:pointer %filament::filament+math+details+t-mat44<float>) mat
                 '%filament::size-t col)))
    (setf (vec4f column row) value)))


(defun create-mat4f (source)
  (let ((mat (iffi:make-intricate-instance '%filament:filament+math+mat4f)))
    (setf (mat4f mat 0 0) (m:mat4 source 0 0)
          (mat4f mat 0 1) (m:mat4 source 1 0)
          (mat4f mat 0 2) (m:mat4 source 2 0)
          (mat4f mat 0 3) (m:mat4 source 3 0)

          (mat4f mat 1 0) (m:mat4 source 0 1)
          (mat4f mat 1 1) (m:mat4 source 1 1)
          (mat4f mat 1 2) (m:mat4 source 2 1)
          (mat4f mat 1 3) (m:mat4 source 3 1)

          (mat4f mat 2 0) (m:mat4 source 0 2)
          (mat4f mat 2 1) (m:mat4 source 1 2)
          (mat4f mat 2 2) (m:mat4 source 2 2)
          (mat4f mat 2 3) (m:mat4 source 3 2)

          (mat4f mat 3 0) (m:mat4 source 0 3)
          (mat4f mat 3 1) (m:mat4 source 1 3)
          (mat4f mat 3 2) (m:mat4 source 2 3)
          (mat4f mat 3 3) (m:mat4 source 3 3))
    mat))


(defun destroy-mat4f (mat)
  (iffi:destroy-intricate-instance '%filament:filament+math+mat4f mat))


(defmacro with-mat4f ((mat source) &body body)
  `(let ((,mat (create-mat4f ,source)))
     (unwind-protect
          (progn ,@body)
       (destroy-mat4f ,mat))))


;;;
;;; MAT3
;;;
(defun mat3f (mat row col)
  (let ((column (%filament::filament+math+details+operator[]
                 '(:pointer %filament::filament+math+details+t-mat33<float>) mat
                 '%filament::size-t col)))
    (vec4f column row)))


(defun (setf mat3f) (value mat row col)
  (let ((column (%filament::filament+math+details+operator[]
                 '(:pointer %filament::filament+math+details+t-mat33<float>) mat
                 '%filament::size-t col)))
    (setf (vec4f column row) value)))


(defun create-mat3f (source)
  (let ((mat (iffi:make-intricate-instance '%filament:filament+math+mat3f)))
    (setf (mat3f mat 0 0) (m:mat3 source 0 0)
          (mat3f mat 0 1) (m:mat3 source 1 0)
          (mat3f mat 0 2) (m:mat3 source 2 0)

          (mat3f mat 1 0) (m:mat3 source 0 1)
          (mat3f mat 1 1) (m:mat3 source 1 1)
          (mat3f mat 1 2) (m:mat3 source 2 1)

          (mat3f mat 2 0) (m:mat3 source 0 2)
          (mat3f mat 2 1) (m:mat3 source 1 2)
          (mat3f mat 2 2) (m:mat3 source 2 2))
    mat))


(defun destroy-mat3f (mat)
  (iffi:destroy-intricate-instance '%filament:filament+math+mat3f mat))


(defmacro with-mat3f ((mat source) &body body)
  `(let ((,mat (create-mat3f ,source)))
     (unwind-protect
          (progn ,@body)
       (destroy-mat3f ,mat))))
