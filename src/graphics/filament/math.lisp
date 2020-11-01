(cl:in-package :alien-works.graphics.filament)

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
