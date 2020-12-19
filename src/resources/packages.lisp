(cl:defpackage :alien-works.resources
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:u :alien-works.utils)
                    (:m :alien-works.math)
                    (:%ai :%assimp))
  (:use :cl)
  (:export #:parse-scene
           #:scene-meshes
           #:scene-images
           #:scene-materials
           #:destroy-scene

           #:mesh-vertex-buffer
           #:mesh-index-buffers

           #:buffer-data
           #:buffer-size
           #:buffer-descriptor

           #:image-name
           #:image-data
           #:image-width
           #:image-height
           #:image-channels))
