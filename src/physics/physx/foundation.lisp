(cl:in-package :alien-works.physics.physx)


(defun make-foundation (allocator error-callback)
  (%physx:px-create-foundation '%physx:physx+px-u32 67174656
                               '(:pointer %physx:physx+px-allocator-callback) allocator
                               '(:pointer %physx:physx+px-error-callback) error-callback))


(defun destroy-foundation (foundation)
  (%physx:physx+release '(:pointer %physx:physx+px-foundation) foundation))


(defun run-with-default-callbacks (action)
  (iffi:with-intricate-instances ((default-allocator %physx:physx+px-default-allocator)
                                  (default-error-callback %physx:physx+px-default-error-callback))
    (funcall action default-allocator default-error-callback)))
