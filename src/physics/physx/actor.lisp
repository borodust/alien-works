(cl:in-package :alien-works.physics.physx)


(defun add-box (physics scene material)
  (iffi:with-intricate-instances ((transform %physx:physx-px-transform
                                             '%physx:physx-px-identity :px-identity)
                                  (geom %physx:physx-px-box-geometry
                                        '%physx::physx-px-real 0.5f0
                                        '%physx::physx-px-real 0.5f0
                                        '%physx::physx-px-real 0.5f0))
    (let* ((flags (iffi:make-intricate-instance
                   '%physx:physx-px-flags<physx-px-shape-flag-enum+unsigned-char>
                   '%physx::physx-px-shape-flag-enum (cffi:foreign-bitfield-value
                                                      '%physx::physx-px-shape-flag-enum
                                                      '(:visualization
                                                        :scene-query-shape
                                                        :simulation-shape))))
           (shape (%physx:physx-create-shape
                   '(:pointer %physx::physx-px-physics) physics
                   '(:pointer %physx::physx-px-geometry) geom
                   '(:pointer %physx::physx-px-material) material
                   ':bool nil
                   '(:pointer %physx::physx-px-shape-flags) flags))

           (body (%physx:physx-create-rigid-dynamic
                  '(:pointer %physx::physx-px-physics) physics
                  '(:pointer %physx::physx-px-transform) transform)))
      (prog1 body
        (%physx:physx-attach-shape
         '(:pointer %physx::physx-px-rigid-actor) body
         '(:pointer %physx::physx-px-shape) shape)
        (%physx:physx-add-actor
         '(:pointer %physx::physx-px-scene) scene
         '(:pointer %physx::physx-px-actor) body
         '(:pointer %physx::physx-px-bvh-structure) (cffi:null-pointer))
        (%physx:physx-release '(:pointer %physx::physx-px-shape) shape)))))


(defun actor-global-pose (transform actor)
  (%physx:physx-get-global-pose '(:pointer %physx::physx-px-transform) transform
                                '(:pointer %physx::physx-px-rigid-actor) actor))
