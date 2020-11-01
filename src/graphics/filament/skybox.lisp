(cl:in-package :alien-works.graphics.filament)

(defun create-color-skybox (engine r g b a)
  (iffi:with-intricate-instance (builder %filament::filament-skybox-builder)
    (%filament:filament-skybox-color
     '(:pointer %filament::filament-skybox-builder) builder
     '(:pointer %filament::filament-math-float4) (create-vec4f r g b a))

    (%filament:filament-skybox-build
     '(:pointer %filament::filament-skybox-builder) builder
     '(:pointer %filament::filament-engine) engine)))
