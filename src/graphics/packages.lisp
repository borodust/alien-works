(cl:defpackage :%alien-works.graphics
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty)
                    (:u :alien-works.utils))
  (:use :cl)
  (:export #:create-engine
           #:destroy-engine
           #:bundled-material-data
           #:bundled-material-size

           #:create-entity
           #:destroy-entity

           #:renderable-primitive-type-enum
           #:with-renderable-builder

           #:with-material-builder

           #:vertex-attribute-enum
           #:vertex-attribute-type-enum
           #:with-vertex-buffer-builder
           #:update-vertex-buffer

           #:index-type-enum
           #:with-index-buffer-builder
           #:update-index-buffer

           #:create-renderer
           #:destroy-renderer
           #:render-view
           #:begin-frame
           #:end-frame

           #:create-swap-chain
           #:destroy-swap-chain

           #:create-camera
           #:destroy-camera

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
           #:with-box))


(cl:defpackage :alien-works.graphics
  (:local-nicknames (:a :alexandria)
                    (:%gx :%alien-works.graphics)
                    (:u :alien-works.utils)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref))
  (:use :cl)
  (:export #:with-engine
           #:render-frame
           #:add-triangle))
