(cl:in-package :alien-works.graphics.filament)


(defun create-entity ()
  (let ((mem (iffi:intricate-alloc '%filament:utils-entity)))
    (%filament:utils-create
     '(:pointer %filament:utils-entity) mem
     '(:pointer %filament::utils-entity-manager) (%filament:utils-entity-manager-get))))


(defun destroy-entity (entity)
  (%filament:utils-destroy
   '(:pointer %filament::utils-entity-manager) (%filament:utils-entity-manager-get)
   '(:pointer %filament:utils-entity) entity)
  (iffi:intricate-free entity))


(defun expand-renderable-manager-builder-function (name args)
  (flet ((explode-function (signature)
           (let ((types (rest signature)))
             (when (/= (length types) (length args))
               (error "Wrong number of arguments: required ~A, but got ~A"
                      types args))
             `(,(first signature) ,@(loop for type in types
                                          for arg in args
                                          append (list type arg))))))
    (ecase name
      (:index-bound-geometry
       (explode-function
        '(%filament:filament-geometry
          (:pointer %filament::filament-renderable-manager-builder)
          '%filament::size-t
          '%filament::filament-renderable-manager-primitive-type
          '(:pointer %filament::filament-vertex-buffer)
          '(:pointer %filament::filament-index-buffer)
          '%filament::size-t
          '%filament::size-t
          '%filament::size-t
          '%filament::size-t)))
      (:count-bound-geometry
       (explode-function
        '(%filament::filament-geometry
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::size-t
          '%filament::filament-renderable-manager-primitive-type
          '(:pointer %filament::filament-vertex-buffer)
          '(:pointer %filament::filament-index-buffer)
          '%filament::size-t
          '%filament::size-t)))
      (:geometry
       (explode-function
        '(%filament::filament-geometry
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::size-t
          '%filament::filament-renderable-manager-primitive-type
          '(:pointer %filament::filament-vertex-buffer)
          '(:pointer %filament::filament-index-buffer))))
      (:material
       (explode-function
        '(%filament::filament-material
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::size-t
          '(:pointer %filament::filament-material-instance))))
      (:bounding-box
       (explode-function
        '(%filament::filament-bounding-box
          '(:pointer %filament::filament-renderable-manager-builder)
          '(:pointer %filament::filament-box))))
      (:layer-mask
       (explode-function
        '(%filament::filament-layer-mask
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::uint8-t
          '%filament::uint8-t)))
      (:priority
       (explode-function
        '(%filament::filament-priority
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::uint8-t)))
      (:culling
       (explode-function
        '(%filament::filament-culling
          '(:pointer %filament::filament-renderable-manager-builder)
          ':bool)))
      (:cast-shadows
       (explode-function
        '(%filament::filament-cast-shadows
          '(:pointer %filament::filament-renderable-manager-builder)
          ':bool)))
      (:receive-shadows
       (explode-function
        '(%filament::filament-receive-shadows
          '(:pointer %filament::filament-renderable-manager-builder)
          ':bool)))
      (:screen-space-contact-shadows
       (explode-function
        '(%filament::filament-screen-space-contact-shadows
          '(:pointer %filament::filament-renderable-manager-builder)
          ':bool)))
      (:transform-skinning
       (explode-function
        '(%filament::filament-skinning
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::size-t
          '(:pointer %filament::filament-math-mat4f))))
      (:bone-skinning
       (explode-function
        '(%filament::filament-skinning
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::size-t
          '(:pointer %filament::filament-renderable-manager-bone))))
      (:skinning
       (explode-function
        '(%filament::filament-skinning
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::size-t)))
      (:morphing
       (explode-function
        '(%filament::filament-morphing
          '(:pointer %filament::filament-renderable-manager-builder)
          ':bool)))
      (:blend-order
       (explode-function
        '(%filament::filament-blend-order
          '(:pointer %filament::filament-renderable-manager-builder)
          '%filament::size-t
          '%filament::uint16-t))))))


(defmacro defrenderable (name-and-opts &body steps)
  (a:with-gensyms (builder)
    (let ((name (first (a:ensure-list name-and-opts))))
      `(defun ,(a:format-symbol (symbol-package name) "~A~A" 'make- name)
           (!::engine !::entity &optional (!::count 1))
         (iffi:with-intricate-instance (,builder %filament:filament-builder
                                                 '%filament:size-t !::count)
           ,@(loop for (name . args) in steps
                   collect (expand-renderable-manager-builder-function name (list* builder args)))
           (%filament:filament-build
            '(:pointer %filament::filament-renderable-manager-builder) ,builder
            '(:pointer %filament::filament-engine) !::engine
            '(:pointer %filament:utils-entity) !::entity))))))
