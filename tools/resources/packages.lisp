(cl:defpackage :alien-works.tools.resources
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref)
                    (:u :alien-works.utils)
                    (:m :alien-works.math)
                    (:%awt.fm :%alien-works.tools.filament)
                    (:%host :%alien-works.host)
                    (:host :alien-works.host)
                    (:%ai :%assimp)
                    (:% :%assimp))
  (:use :cl)
  (:export #:resize-file
           #:parse-scene
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
           #:load-image-from-octet-vector
           #:read-image-into-octet-vector
           #:encode-image-octet-vector-into-png

           #:images-to-cubemap-cross

           #:make-material-provider
           #:destroy-material-provider
           #:make-gltf-loader
           #:destroy-gltf-loader
           #:load-gltf-model-from-byte-vector
           #:destroy-gltf-model
           #:add-scene-gltf-model
           #:remove-scene-gltf-model
           #:gltf-model-resource-names
           #:make-gltf-resource-loader
           #:destroy-gltf-resource-loader
           #:with-gltf-resource-loader
           #:load-gltf-model-resources))
