(cl:in-package :%alien-works.graphics)



;;;
;;; RENDERABLE
;;;
(u:define-enumval-extractor renderable-primitive-type-enum
  %filament:filament-renderable-manager-primitive-type)

(warp-intricate-builder-option renderable-builder :index-bound-geometry
  %filament:filament-geometry
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '%filament:filament-renderable-manager-primitive-type
  '(:pointer %filament:filament-vertex-buffer)
  '(:pointer %filament:filament-index-buffer)
  '%filament:size-t
  '%filament:size-t
  '%filament:size-t
  '%filament:size-t)

(warp-intricate-builder-option renderable-builder :count-bound-geometry
  %filament:filament-geometry
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '%filament:filament-renderable-manager-primitive-type
  '(:pointer %filament:filament-vertex-buffer)
  '(:pointer %filament:filament-index-buffer)
  '%filament:size-t
  '%filament:size-t)

(warp-intricate-builder-option renderable-builder :geometry
  %filament:filament-geometry
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '%filament:filament-renderable-manager-primitive-type
  '(:pointer %filament:filament-vertex-buffer)
  '(:pointer %filament:filament-index-buffer))

(warp-intricate-builder-option renderable-builder :material
  %filament:filament-material
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '(:pointer %filament:filament-material-instance))

(warp-intricate-builder-option renderable-builder :bounding-box
  %filament:filament-bounding-box
  '(:pointer %filament:filament-renderable-manager-builder)
  '(:pointer %filament:filament-box))

(warp-intricate-builder-option renderable-builder :layer-mask
  %filament:filament-layer-mask
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:uint8-t
  '%filament:uint8-t)

(warp-intricate-builder-option renderable-builder :priority
  %filament:filament-priority
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:uint8-t)

(warp-intricate-builder-option renderable-builder :culling
  %filament:filament-culling
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :cast-shadows
  %filament:filament-cast-shadows
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :receive-shadows
  %filament:filament-receive-shadows
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :screen-space-contact-shadows
  %filament:filament-screen-space-contact-shadows
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :transform-skinning
  %filament:filament-skinning
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '(:pointer %filament:filament-math-mat4f))

(warp-intricate-builder-option renderable-builder :bone-skinning
  %filament:filament-skinning
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '(:pointer %filament:filament-renderable-manager-bone))

(warp-intricate-builder-option renderable-builder :skinning
  %filament:filament-skinning
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t)

(warp-intricate-builder-option renderable-builder :morphing
  %filament:filament-morphing
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :blend-order
  %filament:filament-blend-order
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '%filament:uint16-t)


(defmacro with-renderable-builder ((name (&optional count) &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:filament-renderable-manager-builder '%filament:size-t ,count))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-renderable-manager-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine
             '(:pointer %filament:utils-entity) !::entity)))
    (explode-builder name
                     'renderable-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine !::entity)
                     steps
                     body)))
