(cl:in-package :alien-works.physics.physx)


(defun make-cpu-dispatcher (thread-count)
  (%physx:physx+px-default-cpu-dispatcher-create
   '%physx:physx+px-u32 thread-count
   '(:pointer %physx:physx+px-u32) (cffi:null-pointer)))


(defun destroy-cpu-dispatcher (dispatcher)
  (%physx:physx+release
   '(:pointer %physx:physx+px-default-cpu-dispatcher) dispatcher))
