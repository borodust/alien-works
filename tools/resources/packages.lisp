(cl:defpackage :alien-works.tools.resources
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:u :alien-works.utils)
                    (:m :alien-works.math)
                    (:host :alien-works.host)
                    (:cref :cffi-c-ref)
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
           #:mesh-aabb

           #:aabb-min
           #:aabb-max

           #:buffer-data
           #:buffer-size
           #:buffer-descriptor

           #:material-texture

           #:texture-name
           #:texture-channels

           #:load-image
           #:destroy-image

           #:image-name
           #:image-data
           #:image-width
           #:image-height
           #:image-channels

           #:images-to-cubemap-cross))
