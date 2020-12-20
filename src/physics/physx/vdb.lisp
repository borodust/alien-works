(cl:in-package :alien-works.physics.physx)


(u:define-enumval-extractor instrumentation-flag-enum '%physx:physx+px-pvd-instrumentation-flag+enum)


(defun make-pvd (foundation)
  (%physx:physx+px-create-pvd '(:pointer %physx:physx+px-foundation) foundation))


(defun make-instrumentation-flags (flag)
  (iffi:make-intricate-instance
   '%physx:physx+px-flags<physx+px-pvd-instrumentation-flag+enum+unsigned+char>
   '%physx:physx+px-pvd-instrumentation-flag+enum (instrumentation-flag-enum flag)))


(defun destroy-instrumentation-flags (instance)
  (iffi:destroy-intricate-instance
   '%physx:physx+px-flags<physx+px-pvd-instrumentation-flag+enum+unsigned+char>
   instance))


(defun connect-pvd (pvd &key (host "127.0.0.1") (port 5425))
  (let ((transport (%physx:physx+px-default-pvd-socket-transport-create
                    'claw-utils:claw-string host
                    :int port
                    :unsigned-int 10))
        (instrumentation-flags (make-instrumentation-flags :all)))
    (unwind-protect
         (prog1 transport
           (%physx:physx+connect '(:pointer %physx:physx+px-pvd) pvd
                                 '(:pointer %physx:physx+px-pvd-transport) transport
                                 '(:pointer %physx:physx+px-pvd-instrumentation-flags) instrumentation-flags))
      (destroy-instrumentation-flags instrumentation-flags))))


(defun destroy-transport (connection)
  (%physx:physx+release '(:pointer %physx:physx+px-pvd-transport) connection))


(defun destroy-pvd (pvd)
  (%physx:physx+release '(:pointer %physx:physx+px-pvd) pvd))
