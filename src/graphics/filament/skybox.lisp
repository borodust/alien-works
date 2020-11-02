(cl:in-package :alien-works.graphics.filament)


(defun create-color-skybox (engine r g b a)
  (let ((color (create-vec4f r g b a)))
    (with-skybox-builder (%make-skybox (:color color))
      (%make-skybox engine))))
