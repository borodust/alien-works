(cl:in-package :alien-works.graphics.filament)

(defun create-color-skybox (engine r g b a)
  (let ((builder (iffi:make-intricate-instance '%filament::filament-skybox-builder
                                               '%filament::filament-builder)))
    (%filament:filament-skybox-color
     '(:pointer %filament::filament-skybox-builder) builder
     '(:pointer %filament::filament-math-float4) (create-vec4f r g b a))

    (prog1 (%filament:filament-skybox-build
            '(:pointer %filament::filament-skybox-builder) builder
            '(:pointer %filament::filament-engine) engine)
      (iffi:destroy-intricate-instance '%filament::filament-skybox-builder
                                       '%filament::filament-skybox-~builder
                                       builder))))
