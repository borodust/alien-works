(cl:defpackage :%alien-works.graphics
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty)
                    (:u :alien-works.utils)
                    (:m :alien-works.math))
  (:use :cl)
  (:export #:create-engine
           #:destroy-engine

           #:with-material-builder
           #:default-material-instance
           #:make-material-instance
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
           #:update-camera-model-matrix

           #:create-view
           #:destroy-view
           #:view-camera
           #:view-scene
           #:view-anti-aliasing
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

           #:with-mat4f

           #:with-vec4f
           #:with-vec3f

           #:with-light-builder
           #:light-builder-cast-shadows
           #:light-builder-shadow-options
           #:light-builder-cast-light
           #:light-builder-position
           #:light-builder-direction
           #:light-builder-color
           #:light-builder-intensity
           #:light-builder-intensity-efficiency
           #:light-builder-falloff
           #:light-builder-spot-light-cone
           #:light-builder-sun-angular-radius
           #:light-builder-sun-halo-size
           #:light-builder-sun-halo-falloff

           #:texture-sampler-type-enum
           #:texture-internal-format-enum
           #:texture-cubemap-face-enum
           #:texture-usage-enum
           #:texture-swizzle-enum
           #:with-texture-builder
           #:destroy-texture
           #:texture-builder-width
           #:texture-builder-height
           #:texture-builder-depth
           #:texture-builder-levels
           #:texture-builder-sampler
           #:texture-builder-format
           #:texture-builder-usage
           #:texture-builder-swizzle
           #:texture-builder-import
           #:update-texture-image
           #:update-texture-subimage
           #:update-texture-array-subimage
           #:update-cubemap-images
           #:generate-texture-mipmaps
           #:generate-texture-prefilter-mipmaps

           #:pixel-format-enum
           #:pixel-type-enum
           #:pixel-compressed-type-enum
           #:make-pixel-buffer
           #:make-compressed-pixel-buffer
           #:destory-pixel-buffer))


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

           #:add-scene-entity

           #:transform-camera

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
           #:transform-entity

           #:make-light
           #:destroy-light

           #:make-texture
           #:update-texture-image
           #:destroy-texture

           #:make-pixel-buffer
           #:make-compressed-pixel-buffer
           #:destroy-pixel-buffer

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
           #:.blend-order

           #:.cast-light
           #:.position
           #:.direction
           #:.color
           #:.intensity
           #:.intensity-efficiency
           #:.falloff
           #:.spot-light-cone
           #:.sun-angular-radius
           #:.sun-halo-size
           #:.sun-halo-falloff

           #:.width
           #:.height
           #:.depth
           #:.levels
           #:.sampler
           #:.format
           #:.usage
           #:.swizzle))
