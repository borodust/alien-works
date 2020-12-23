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
           #:mesh-material-index
           #:mesh-material

           #:buffer-data
           #:buffer-size
           #:buffer-descriptor

           #:material-texture

           #:texture-name

           #:load-image
           #:destroy-image

           #:image-name
           #:image-data
           #:image-width
           #:image-height
           #:image-channels))
