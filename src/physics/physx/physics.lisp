(cl:in-package :alien-works.physics.physx)


(defun make-physics (foundation pvd scale)
  (%physx:px-create-physics '%physx:physx-px-u32 67174656
                            '(:pointer %physx::physx-px-foundation) foundation
                            '(:pointer %physx::physx-px-tolerances-scale) scale
                            :bool t
                            '(:pointer %physx::physx-px-pvd) pvd))


(defun destroy-physics (physics)
  (%physx:physx-release '(:pointer %physx::physx-px-physics) physics))
