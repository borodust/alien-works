(cl:in-package :alien-works.graphics.filament)


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


;;;
;;; RENDERABLE
;;;
(defun expand-renderable-manager-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:index-bound-geometry
       (%explode-function
        '(%filament:filament-geometry
          (:pointer %filament:filament-renderable-manager-builder)
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


(defmacro with-renderable-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament-renderable-manager-builder '%filament:size-t !::count))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-renderable-manager-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine
             '(:pointer %filament:utils-entity) !::entity)))
    (explode-builder name
                     #'expand-renderable-manager-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine !::entity &optional (!::count 1))
                     steps
                     body)))

;;;
;;; VERTEX BUFFER
;;;
(defun expand-vertex-buffer-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:buffer-count
       (%explode-function
        '(%filament:filament-buffer-count
          '(:pointer %filament::filament-vertex-buffer-builder)
          '%filament:uint8-t)))
      (:vertex-count
       (%explode-function
        '(%filament:filament-vertex-count
          '(:pointer %filament::filament-vertex-buffer-builder)
          '%filament:uint32-t)))
      (:attribute
       (%explode-function
        '(%filament:filament-attribute
          '(:pointer %filament::filament-vertex-buffer-builder)
          '%filament:filament-vertex-attribute
          '%filament:uint8-t
          '%filament:filament-vertex-buffer-attribute-type
          '%filament:uint32-t
          '%filament:uint8-t)))
      (:normalized
       (%explode-function
        '(%filament:filament-normalized
          '(:pointer %filament::filament-vertex-buffer-builder)
          '%filament:filament-vertex-attribute
          ':bool))))))


(defmacro with-vertex-buffer-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament-vertex-buffer-builder))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-vertex-buffer-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine)))
    (explode-builder name
                     #'expand-vertex-buffer-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


;;;
;;; INDEX BUFFER
;;;
(defun expand-index-buffer-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:index-count
       (%explode-function
        '(%filament:filament-index-count
          '(:pointer %filament::filament-index-buffer-builder)
          '%filament:uint32-t)))
      (:buffer-type
       (%explode-function
        '(%filament:filament-buffer-type
          '(:pointer %filament::filament-index-buffer-builder)
          '%filament:filament-index-buffer-index-type))))))


(defmacro with-index-buffer-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament-index-buffer-builder))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-index-buffer-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine)))
    (explode-builder name
                     #'expand-index-buffer-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))

;;;
;;; MATERIAL
;;;
(defun expand-material-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:package
       (%explode-function
        '(%filament:filament-package
          '(:pointer %filament::filament-material-builder)
          '(:pointer :void)
          '%filament:size-t))))))


(defmacro with-material-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament-material-builder))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-material-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine)))
    (explode-builder name
                     #'expand-material-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


;;;
;;; SKYBOX
;;;
(defun expand-skybox-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:environment
       (%explode-function
        '(%filament:filament-environment
          '(:pointer %filament:filament-skybox-builder)
          '(:pointer %filament:filament-texture))))
      (:show-sun
       (%explode-function
        '(%filament:filament-show-sun
          '(:pointer %filament:filament-skybox-builder)
          ':bool)))
      (:intensity
       (%explode-function
        '(%filament:filament-intensity
          '(:pointer %filament:filament-skybox-builder)
          ':float)))
      (:color
       (%explode-function
        '(%filament:filament-color
          '(:pointer %filament:filament-skybox-builder)
          '(:pointer %filament:filament-math-float4)))))))


(defmacro with-skybox-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament-skybox-builder))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-skybox-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine)))
    (explode-builder name
                     #'expand-skybox-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))
