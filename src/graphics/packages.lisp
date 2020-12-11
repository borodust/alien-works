(cl:defpackage :%alien-works.graphics
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty)
                    (:u :alien-works.utils)
                    (:m :alien-works.math))
  (:use :cl)
  (:export #:create-engine
           #:destroy-engine

           #:with-material-builder
           #:material-default-instance
           #:bundled-material-data
           #:bundled-material-size
           #:parse-material
           #:destroy-material
           #:material-data
           #:material-size
           #:with-parsed-material

           #:create-entity
           #:destroy-entity
           #:destroy-engine-entity

           #:renderable-primitive-type-enum
           #:renderable-builder-index-bound-geometry
           #:renderable-builder-count-bound-geometry
           #:renderable-builder-geometry
           #:renderable-builder-material
           #:renderable-builder-bounding-box
           #:renderable-builder-layer-mask
           #:renderable-builder-priority
           #:renderable-builder-culling
           #:renderable-builder-cast-shadows
           #:renderable-builder-receive-shadows
           #:renderable-builder-screen-space-contact-shadows
           #:renderable-builder-transform-skinning
           #:renderable-builder-bone-skinning
           #:renderable-builder-skinning
           #:renderable-builder-morphing
           #:renderable-builder-blend-order
           #:with-renderable-builder

           #:vertex-attribute-enum
           #:vertex-attribute-type-enum
           #:vertex-buffer-builder-buffer-count
           #:vertex-buffer-builder-vertex-count
           #:vertex-buffer-builder-attribute
           #:vertex-buffer-builder-normalized
           #:with-vertex-buffer-builder
           #:destroy-vertex-buffer
           #:update-vertex-buffer

           #:index-type-enum
           #:index-buffer-builder-index-count
           #:index-buffer-builder-buffer-type
           #:with-index-buffer-builder
           #:destroy-index-buffer
           #:update-index-buffer

           #:create-renderer
           #:destroy-renderer
           #:render-view
           #:begin-frame
           #:end-frame

           #:create-swap-chain
           #:destroy-swap-chain

           #:projection-enum
           #:create-camera
           #:destroy-camera
           #:update-camera-projection
           #:update-camera-fov-projection
           #:update-camera-lens-projection

           #:create-view
           #:destroy-view
           #:view-camera
           #:view-scene
           #:view-post-processing-enabled-p
           #:update-view-viewport

           #:create-scene
           #:destroy-scene
           #:scene-skybox
           #:add-scene-entity

           #:with-skybox-builder
           #:create-color-skybox

           #:create-box
           #:destroy-box
           #:with-box

           #:transform-manager
           #:with-transform-instance
           #:transform

           #:with-mat4f))


(cl:defpackage :alien-works.graphics
  (:local-nicknames (:a :alexandria)
                    (:%gx :%alien-works.graphics)
                    (:u :alien-works.utils)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref)
                    (:m :alien-works.math))
  (:use :cl)
  (:export #:with-engine
           #:render-frame

           #:add-triangle
           #:rotate-triangle

           #:make-material
           #:destroy-material

           #:make-vertex-buffer
           #:destroy-vertex-buffer
           #:fill-vertex-buffer

           #:make-index-buffer
           #:destroy-index-buffer
           #:fill-index-buffer

           #:make-renderable
           #:destroy-renderable
           #:add-renderable

           #:.material

           #:.vertex-count
           #:.attribute
           #:.normalized

           #:.index-count
           #:.type

           #:.geometry
           #:.count-bound-geometry
           #:.index-bound-geometry
           #:.material
           #:.bounding-box
           #:.layer-mask
           #:.priority
           #:.culling
           #:.cast-shadows
           #:.receive-shadows
           #:.screen-space-contact-shadows
           #:.transform-skinning
           #:.bone-skinning
           #:.skinning
           #:.morphing
           #:.blend-order))
