(cl:defpackage :alien-works.resources
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:u :alien-works.utils)
                    (:m :alien-works.math)
                    (:%ai :%assimp))
  (:use :cl)
  (:export #:parse-scene
           #:scene-meshes
           #:destroy-scene

           #:mesh-vertex-buffer
           #:mesh-index-buffers

           #:buffer-data
           #:buffer-size
           #:buffer-descriptor))
