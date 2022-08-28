(cl:in-package :%alien-works.filament)


(defun renderable-manager (engine)
  (%filament:get-renderable-manager
   '(claw-utils:claw-pointer %filament::engine) engine))

;;;
;;; RENDERABLE
;;;
(u:define-enumval-extractor renderable-primitive-type-enum
  %filament:renderable-manager+primitive-type)

(warp-intricate-builder-option renderable-builder :index-bound-geometry
  %filament:geometry
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:size-t
  '%filament:renderable-manager+primitive-type
  '(claw-utils:claw-pointer %filament:vertex-buffer)
  '(claw-utils:claw-pointer %filament:index-buffer)
  '%filament:size-t
  '%filament:size-t
  '%filament:size-t
  '%filament:size-t)

(warp-intricate-builder-option renderable-builder :count-bound-geometry
  %filament:geometry
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:size-t
  '%filament:renderable-manager+primitive-type
  '(claw-utils:claw-pointer %filament:vertex-buffer)
  '(claw-utils:claw-pointer %filament:index-buffer)
  '%filament:size-t
  '%filament:size-t)

(warp-intricate-builder-option renderable-builder :geometry
  %filament:geometry
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:size-t
  '%filament:renderable-manager+primitive-type
  '(claw-utils:claw-pointer %filament:vertex-buffer)
  '(claw-utils:claw-pointer %filament:index-buffer))

(warp-intricate-builder-option renderable-builder :material
  %filament:material
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:size-t
  '(claw-utils:claw-pointer %filament:material-instance))

(warp-intricate-builder-option renderable-builder :bounding-box
  %filament:bounding-box
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '(claw-utils:claw-pointer %filament:box))

(warp-intricate-builder-option renderable-builder :layer-mask
  %filament:layer-mask
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:uint8-t
  '%filament:uint8-t)

(warp-intricate-builder-option renderable-builder :priority
  %filament:priority
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:uint8-t)

(warp-intricate-builder-option renderable-builder :culling
  %filament:culling
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :cast-shadows
  %filament:cast-shadows
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :receive-shadows
  %filament:receive-shadows
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :screen-space-contact-shadows
  %filament:screen-space-contact-shadows
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  ':bool)

(warp-intricate-builder-option renderable-builder :transform-skinning
  %filament:skinning
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:size-t
  '(claw-utils:claw-pointer %filament:math+mat4f))

(warp-intricate-builder-option renderable-builder :bone-skinning
  %filament:skinning
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:size-t
  '(claw-utils:claw-pointer %filament:renderable-manager+bone))

(warp-intricate-builder-option renderable-builder :skinning
  %filament:skinning
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:size-t)

(warp-intricate-builder-option renderable-builder :blend-order
  %filament:blend-order
  '(claw-utils:claw-pointer %filament:renderable-manager+builder)
  '%filament:size-t
  '%filament:uint16-t)


(defmacro with-renderable-builder ((name (&optional count) &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:renderable-manager+builder '%filament:size-t ,count))
         (build-expander (builder)
           `(%filament:build
             '(claw-utils:claw-pointer %filament:renderable-manager+builder) ,builder
             '(claw-utils:claw-pointer %filament:engine) !::engine
             '(claw-utils:claw-pointer %filament:utils+entity) !::entity)))
    (explode-builder name
                     'renderable-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine !::entity)
                     steps
                     body)))


(defmacro with-renderable-instance ((instance entity) renderable-manager &body body)
  `(iffi:with-intricate-instance (,instance %filament::renderable-manager+instance)
     (%filament:get-instance
      :const
      '(claw-utils:claw-pointer %filament::renderable-manager+instance) ,instance
      '(claw-utils:claw-pointer %filament::renderable-manager) ,renderable-manager
      '(claw-utils:claw-pointer %filament::utils+entity) ,entity)
     ,@body))


(defun renderable-material-instance (renderable-manager instance layer)
  (%filament:get-material-instance-at
   :const
   '(claw-utils:claw-pointer %filament::renderable-manager) renderable-manager
   '(claw-utils:claw-pointer %filament::renderable-manager+instance) instance
   '%filament::size-t layer))


(defun (setf renderable-material-instance) (material-instance
                                            renderable-manager
                                            instance
                                            layer)
  (%filament:set-material-instance-at
   '(claw-utils:claw-pointer %filament::renderable-manager) renderable-manager
   '(claw-utils:claw-pointer %filament::renderable-manager+instance) instance
   '%filament::size-t layer
   '(claw-utils:claw-pointer %filament::material-instance) material-instance)
  material-instance)
