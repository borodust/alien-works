(cl:defpackage :alien-works.physics.physx
  (:use :cl)
  (:export #:with-vec3
           #:x
           #:y
           #:z
           #:w

           #:with-scale

           #:with-transform
           #:transform-position

           #:make-foundation
           #:destroy-foundation

           #:with-scene-descriptor
           #:scene-descriptor-valid-p
           #:gravity

           #:make-scene
           #:simulate-scene
           #:finish-simulation
           #:destroy-scene

           #:make-cpu-dispatcher
           #:destroy-cpu-dispatcher

           #:make-material
           #:destroy-material

           #:make-pvd
           #:connect-pvd
           #:destroy-pvd

           #:make-physics
           #:destroy-physics

           #:make-transport
           #:destroy-transport

           #:add-box
           #:actor-global-pose

           #:run-with-default-callbacks))


(cl:defpackage :alien-works.physics
  (:local-nicknames (:px :alien-works.physics.physx))
  (:use :cl))
