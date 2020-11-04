(cl:in-package :%alien-works.graphics)


(defun create-color-skybox (engine r g b a)
  (with-vec4f (color r g b a)
    (with-skybox-builder (%make-skybox (:color color))
      (%make-skybox engine))))
