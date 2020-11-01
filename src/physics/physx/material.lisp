(cl:in-package :alien-works.physics.physx)


(defun make-material (physics)
  (%physx::physx-create-material '(:pointer %physx::physx-px-physics) physics
                                 '%physx::physx-px-real 0.5f0
                                 '%physx::physx-px-real 0.5f0
                                 '%physx::physx-px-real 0.6f0))


(defun destroy-material (material)
  (%physx:physx-release '(:pointer %physx::physx-px-material) material))
