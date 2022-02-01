(cl:defpackage :alien-works.tools.resources
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref)
                    (:u :alien-works.utils)
                    (:m :alien-works.math)
                    (:host :alien-works.host)
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

           #:images-to-cubemap-cross

           #:make-material-provider
           #:destroy-material-provider
           #:make-gltf-loader
           #:destroy-gltf-loader
           #:load-text-gltf-model-from-byte-vector
           #:load-binary-gltf-model-from-byte-vector
           #:destroy-gltf-model
           #:add-scene-gltf-model
           #:remove-scene-gltf-model
           #:gltf-model-resource-names
           #:make-gltf-resource-loader
           #:destroy-gltf-resource-loader
           #:load-gltf-model-resources))
