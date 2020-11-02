(cl:defpackage :alien-works.graphics.filament
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty))
  (:use :cl)
  (:export #:create-engine
           #:destroy-engine

           #:create-entity
           #:destroy-entity

           #:with-renderable-builder
           #:with-vertex-buffer-builder
           #:with-index-buffer-builder
           #:with-material-builder

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

           #:with-skybox-builder
           #:create-color-skybox))


(cl:defpackage :alien-works.graphics
  (:local-nicknames (:a :alexandria)
                    (:fm ::alien-works.graphics.filament))
  (:use :cl)
  (:export #:with-engine
           #:render-frame))
