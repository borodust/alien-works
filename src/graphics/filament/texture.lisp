(cl:in-package :%alien-works.graphics)


(warp-intricate-builder-option texture-builder :width
    %filament:filament-width
  '(:pointer %filament:filament-texture-builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :height
    %filament:filament-height
  '(:pointer %filament:filament-texture-builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :depth
    %filament:filament-depth
  '(:pointer %filament:filament-texture-builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :levels
    %filament:filament-levels
  '(:pointer %filament:filament-texture-builder)
  '%filament:uint8-t)

(warp-intricate-builder-option texture-builder :sampler
    %filament:filament-sampler
  '(:pointer %filament:filament-texture-builder)
  '%filament:filament-texture-sampler)

(warp-intricate-builder-option texture-builder :format
    %filament:filament-format
  '(:pointer %filament:filament-texture-builder)
  '%filament:filament-texture-internal-format)

(warp-intricate-builder-option texture-builder :usage
    %filament:filament-usage
  '(:pointer %filament:filament-texture-builder)
  '%filament:filament-texture-usage)

(warp-intricate-builder-option texture-builder :swizzle
    %filament:filament-swizzle
  '(:pointer %filament:filament-texture-builder)
  '%filament:filament-texture-swizzle
  '%filament:filament-texture-swizzle
  '%filament:filament-texture-swizzle
  '%filament:filament-texture-swizzle)

(warp-intricate-builder-option texture-builder :import
    %filament:filament-import
  '(:pointer %filament:filament-texture-builder)
  '%filament:intptr-t)


(defmacro with-texture-builder ((name &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:filament-texture-builder))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-texture-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine)))
    (explode-builder name
                     'texture-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun update-texture-image (engine texture level pixel-buffer)
  (%filament::filament-set-image
   '(:pointer %filament::filament-texture) texture
   '(:pointer %filament::filament-engine) engine
   '%filament::size-t level
   '(:pointer %filament::filament-texture-pixel-buffer-descriptor) pixel-buffer))


(defun update-texture-subimage (engine texture level x-offset y-offset width height pixel-buffer)
  (%filament::filament-set-image
   '(:pointer %filament::filament-texture) texture
   '(:pointer %filament::filament-engine) engine
   '%filament::size-t level
   '%filament::uint32-t x-offset
   '%filament::uint32-t y-offset
   '%filament::uint32-t width
   '%filament::uint32-t height
   '(:pointer %filament::filament-texture-pixel-buffer-descriptor) pixel-buffer))


(defun update-texture-array-subimage (engine texture level x-offset y-offset z-offset
                                      width height depth
                                      pixel-buffer)
  (%filament::filament-set-image
   '(:pointer %filament::filament-texture) texture
   '(:pointer %filament::filament-engine) engine
   '%filament::size-t level
   '%filament::uint32-t x-offset
   '%filament::uint32-t y-offset
   '%filament::uint32-t z-offset
   '%filament::uint32-t width
   '%filament::uint32-t height
   '%filament::uint32-t depth
   '(:pointer %filament::filament-texture-pixel-buffer-descriptor) pixel-buffer))


(defun update-cubemap-images (engine texture level pixel-buffer face-offsets)
  (%filament::filament-set-image
   '(:pointer %filament::filament-texture) texture
   '(:pointer %filament::filament-engine) engine
   '%filament::size-t level
   '(:pointer %filament::filament-texture-pixel-buffer-descriptor) pixel-buffer
   '(:pointer %filament::filament-texture-face-offsets) face-offsets))


(defun generate-texture-mipmaps (engine texture)
  (%filament:filament-generate-mipmaps
   '(:pointer %filament::filament-texture) texture
   '(:pointer %filament::filament-engine) engine))


(defun generate-texture-prefilter-mipmaps (engine texture pixel-buffer face-offsets
                                           &optional prefilter-options)
  (%filament:filament-generate-prefilter-mipmap
   '(:pointer %filament::filament-texture) texture
   '(:pointer %filament::filament-engine) engine
   '(:pointer %filament::filament-texture-pixel-buffer-descriptor) pixel-buffer
   '(:pointer %filament::filament-texture-face-offsets) face-offsets
   '(:pointer %filament::filament-texture-prefilter-options) prefilter-options))
