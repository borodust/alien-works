(cl:in-package :%alien-works.graphics)


(defun create-entity ()
  (let ((mem (iffi:intricate-alloc '%filament:utils-entity)))
    (%filament:utils-create
     '(:pointer %filament:utils-entity) mem
     '(:pointer %filament:utils-entity-manager) (%filament:utils-entity-manager-get))))


(defun destroy-entity (entity)
  (%filament:utils-destroy
   '(:pointer %filament:utils-entity-manager) (%filament:utils-entity-manager-get)
   '(:pointer %filament:utils-entity) entity)
  (iffi:intricate-free entity))


(defun destroy-engine-entity (engine entity)
  (%filament:filament-destroy
   '(:pointer %filament:filament-engine) engine
   '(:pointer %filament:utils-entity) entity)
  (iffi:intricate-free entity))


;;;
;;; RENDERABLE
;;;
(u:define-enumval-extractor renderable-primitive-type-enum
  %filament:filament-renderable-manager-primitive-type)

(defun expand-renderable-manager-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:index-bound-geometry
       (%explode-function
        '(%filament:filament-geometry
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:size-t
          '%filament:filament-renderable-manager-primitive-type
          '(:pointer %filament:filament-vertex-buffer)
          '(:pointer %filament:filament-index-buffer)
          '%filament:size-t
          '%filament:size-t
          '%filament:size-t
          '%filament:size-t)))
      (:count-bound-geometry
       (%explode-function
        '(%filament:filament-geometry
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:size-t
          '%filament:filament-renderable-manager-primitive-type
          '(:pointer %filament:filament-vertex-buffer)
          '(:pointer %filament:filament-index-buffer)
          '%filament:size-t
          '%filament:size-t)))
      (:geometry
       (%explode-function
        '(%filament:filament-geometry
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:size-t
          '%filament:filament-renderable-manager-primitive-type
          '(:pointer %filament:filament-vertex-buffer)
          '(:pointer %filament:filament-index-buffer))))
      (:material
       (%explode-function
        '(%filament:filament-material
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:size-t
          '(:pointer %filament:filament-material-instance))))
      (:bounding-box
       (%explode-function
        '(%filament:filament-bounding-box
          '(:pointer %filament:filament-renderable-manager-builder)
          '(:pointer %filament:filament-box))))
      (:layer-mask
       (%explode-function
        '(%filament:filament-layer-mask
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:uint8-t
          '%filament:uint8-t)))
      (:priority
       (%explode-function
        '(%filament:filament-priority
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:uint8-t)))
      (:culling
       (%explode-function
        '(%filament:filament-culling
          '(:pointer %filament:filament-renderable-manager-builder)
          ':bool)))
      (:cast-shadows
       (%explode-function
        '(%filament:filament-cast-shadows
          '(:pointer %filament:filament-renderable-manager-builder)
          ':bool)))
      (:receive-shadows
       (%explode-function
        '(%filament:filament-receive-shadows
          '(:pointer %filament:filament-renderable-manager-builder)
          ':bool)))
      (:screen-space-contact-shadows
       (%explode-function
        '(%filament:filament-screen-space-contact-shadows
          '(:pointer %filament:filament-renderable-manager-builder)
          ':bool)))
      (:transform-skinning
       (%explode-function
        '(%filament:filament-skinning
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:size-t
          '(:pointer %filament:filament-math-mat4f))))
      (:bone-skinning
       (%explode-function
        '(%filament:filament-skinning
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:size-t
          '(:pointer %filament:filament-renderable-manager-bone))))
      (:skinning
       (%explode-function
        '(%filament:filament-skinning
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:size-t)))
      (:morphing
       (%explode-function
        '(%filament:filament-morphing
          '(:pointer %filament:filament-renderable-manager-builder)
          ':bool)))
      (:blend-order
       (%explode-function
        '(%filament:filament-blend-order
          '(:pointer %filament:filament-renderable-manager-builder)
          '%filament:size-t
          '%filament:uint16-t))))))


(warp-intricate-function renderable-builder-index-bound-geometry
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

(warp-intricate-function renderable-builder-count-bound-geometry
    %filament:filament-geometry
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '%filament:filament-renderable-manager-primitive-type
  '(:pointer %filament:filament-vertex-buffer)
  '(:pointer %filament:filament-index-buffer)
  '%filament:size-t
  '%filament:size-t)

(warp-intricate-function renderable-builder-geometry
    %filament:filament-geometry
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '%filament:filament-renderable-manager-primitive-type
  '(:pointer %filament:filament-vertex-buffer)
  '(:pointer %filament:filament-index-buffer))

(warp-intricate-function renderable-builder-material
    %filament:filament-material
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '(:pointer %filament:filament-material-instance))

(warp-intricate-function renderable-builder-bounding-box
    %filament:filament-bounding-box
  '(:pointer %filament:filament-renderable-manager-builder)
  '(:pointer %filament:filament-box))

(warp-intricate-function renderable-builder-layer-mask
    %filament:filament-layer-mask
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:uint8-t
  '%filament:uint8-t)

(warp-intricate-function renderable-builder-priority
    %filament:filament-priority
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:uint8-t)

(warp-intricate-function renderable-builder-culling
    %filament:filament-culling
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-function renderable-builder-cast-shadows
    %filament:filament-cast-shadows
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-function renderable-builder-receive-shadows
    %filament:filament-receive-shadows
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-function renderable-builder-screen-space-contact-shadows
    %filament:filament-screen-space-contact-shadows
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-function renderable-builder-transform-skinning
    %filament:filament-skinning
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '(:pointer %filament:filament-math-mat4f))

(warp-intricate-function renderable-builder-bone-skinning
    %filament:filament-skinning
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t
  '(:pointer %filament:filament-renderable-manager-bone))

(warp-intricate-function renderable-builder-skinning
    %filament:filament-skinning
  '(:pointer %filament:filament-renderable-manager-builder)
  '%filament:size-t)

(warp-intricate-function renderable-builder-morphing
    %filament:filament-morphing
  '(:pointer %filament:filament-renderable-manager-builder)
  ':bool)

(warp-intricate-function renderable-builder-blend-order
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
                     #'expand-renderable-manager-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine !::entity)
                     steps
                     body)))
