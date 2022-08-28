(cl:in-package :%alien-works.filament)


(u:define-enumval-extractor texture-sampler-type-enum %filament:backend+sampler-type)
(u:define-enumval-extractor texture-internal-format-enum %filament:texture+internal-format)
(u:define-enumval-extractor texture-cubemap-face-enum %filament:texture+cubemap-face)
(u:define-enumval-extractor texture-usage-enum %filament:texture+usage)
(u:define-enumval-extractor texture-swizzle-enum %filament:texture+swizzle)

(u:define-enumval-extractor pixel-format-enum %filament:backend+pixel-data-format)
(u:define-enumval-extractor pixel-type-enum %filament:backend+pixel-data-type)
(u:define-enumval-extractor pixel-compressed-type-enum
  %filament:backend+compressed-pixel-data-type)


(warp-intricate-builder-option texture-builder :width
    %filament:width
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :height
    %filament:height
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :depth
    %filament:depth
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :levels
    %filament:levels
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:uint8-t)

(warp-intricate-builder-option texture-builder :sampler
    %filament:sampler
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:texture+sampler)

(warp-intricate-builder-option texture-builder :format
    %filament:format
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:texture+internal-format)

(warp-intricate-builder-option texture-builder :usage
    %filament:usage
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:texture+usage)

(warp-intricate-builder-option texture-builder :swizzle
    %filament:swizzle
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:texture+swizzle
  '%filament:texture+swizzle
  '%filament:texture+swizzle
  '%filament:texture+swizzle)

(warp-intricate-builder-option texture-builder :import
    %filament:import
  '(claw-utils:claw-pointer %filament:texture+builder)
  '%filament:intptr-t)


(defmacro with-texture-builder ((name &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:texture+builder))
         (build-expander (builder)
           `(%filament:build
             '(claw-utils:claw-pointer %filament:texture+builder) ,builder
             '(claw-utils:claw-pointer %filament:engine) !::engine)))
    (explode-builder name
                     'texture-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-texture (engine texture)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::texture) texture))


(defun update-texture-image (engine texture level pixel-buffer)
  (%filament:set-image
   :const
   '(claw-utils:claw-pointer %filament:texture) texture
   '(claw-utils:claw-pointer %filament:engine) engine
   '%filament:size-t level
   '(claw-utils:claw-pointer %filament:texture+pixel-buffer-descriptor) pixel-buffer))


(defun update-texture-subimage (engine texture level x-offset y-offset width height pixel-buffer)
  (%filament:set-image
   :const
   '(claw-utils:claw-pointer %filament:texture) texture
   '(claw-utils:claw-pointer %filament:engine) engine
   '%filament:size-t level
   '%filament:uint32-t x-offset
   '%filament:uint32-t y-offset
   '%filament:uint32-t width
   '%filament:uint32-t height
   '(claw-utils:claw-pointer %filament:texture+pixel-buffer-descriptor) pixel-buffer))


(defun update-texture-array-subimage (engine texture level x-offset y-offset z-offset
                                      width height depth
                                      pixel-buffer)
  (%filament:set-image
   :const
   '(claw-utils:claw-pointer %filament:texture) texture
   '(claw-utils:claw-pointer %filament:engine) engine
   '%filament:size-t level
   '%filament:uint32-t x-offset
   '%filament:uint32-t y-offset
   '%filament:uint32-t z-offset
   '%filament:uint32-t width
   '%filament:uint32-t height
   '%filament:uint32-t depth
   '(claw-utils:claw-pointer %filament:texture+pixel-buffer-descriptor) pixel-buffer))


(defmacro with-face-offsets ((val) &body body)
  `(iffi:with-intricate-instance (,val %filament:texture+face-offsets)
     ,@body))


(defun face-offset (offsets idx)
  (%filament:operator[]
   :const
   '(claw-utils:claw-pointer %filament:texture+face-offsets) offsets
   '%filament::size-t idx))


(defun (setf face-offset) (value offsets idx)
  (let ((ptr (%filament:operator[]
              '(claw-utils:claw-pointer %filament:texture+face-offsets) offsets
              '%filament::size-t idx)))
    (setf (cffi:mem-ref ptr '%filament:texture+face-offsets+size-type) value)))


(defun update-cubemap-images (engine texture level pixel-buffer face-offsets)
  (%filament:set-image
   :const
   '(claw-utils:claw-pointer %filament:texture) texture
   '(claw-utils:claw-pointer %filament:engine) engine
   '%filament:size-t level
   '(claw-utils:claw-pointer %filament:texture+pixel-buffer-descriptor) pixel-buffer
   '(claw-utils:claw-pointer %filament:texture+face-offsets) face-offsets))


(defun generate-texture-mipmaps (engine texture)
  (%filament:generate-mipmaps
   :const
   '(claw-utils:claw-pointer %filament:texture) texture
   '(claw-utils:claw-pointer %filament:engine) engine))


(defun generate-texture-prefilter-mipmaps (engine texture pixel-buffer face-offsets
                                           &optional prefilter-options)
  (%filament:generate-prefilter-mipmap
   '(claw-utils:claw-pointer %filament:texture) texture
   '(claw-utils:claw-pointer %filament:engine) engine
   '(claw-utils:claw-pointer %filament:texture+pixel-buffer-descriptor) pixel-buffer
   '(claw-utils:claw-pointer %filament:texture+face-offsets) face-offsets
   '(claw-utils:claw-pointer %filament:texture+prefilter-options) prefilter-options))


;;;
;;; PIXEL BUFFER
;;;
(defvar *buffer-release-callback-table* (make-hash-table))


(defun register-release-buffer-callback (release-callback)
  (if release-callback
      (let ((identity-byte (cffi:foreign-alloc :char)))
        (setf
         (gethash (cffi:pointer-address identity-byte) *buffer-release-callback-table*)
         release-callback)
        identity-byte)
      (cffi:null-pointer)))


(cffi:defcallback release-buffer-callback claw-utils:claw-pointer ((data claw-utils:claw-pointer)
                                                    (size %filament:size-t)
                                                    (user claw-utils:claw-pointer))
  (declare (ignore data size))
  (a:when-let (callback (gethash (cffi:pointer-address user) *buffer-release-callback-table*))
    (unwind-protect
         (funcall callback)
      (cffi:foreign-free user)
      (remhash (cffi:pointer-address user) *buffer-release-callback-table*)))
  (cffi:null-pointer))


(defun make-pixel-buffer (data-ptr data-size pixel-format pixel-type &optional release-callback)
  (iffi:make-intricate-instance
   '%filament:backend+pixel-buffer-descriptor
   '(claw-utils:claw-pointer :void) data-ptr
   '%filament:size-t data-size
   '%filament:backend+pixel-buffer-descriptor+pixel-data-format pixel-format
   '%filament:backend+pixel-buffer-descriptor+pixel-data-type pixel-type
   '%filament:backend+buffer-descriptor+callback (cffi:callback release-buffer-callback)
   '(claw-utils:claw-pointer :void) (register-release-buffer-callback release-callback)))


(defun make-compressed-pixel-buffer (data-ptr data-size compressed-size compressed-pixel-type
                                     &optional release-callback)
  (iffi:make-intricate-instance
   '%filament:backend+pixel-buffer-descriptor
   '(claw-utils:claw-pointer :void) data-ptr
   '%filament:size-t data-size
   '%filament:backend+compressed-pixel-data-type compressed-pixel-type
   '%filament:uint32-t compressed-size
   '%filament:backend+buffer-descriptor+callback (cffi:callback release-buffer-callback)
   '(claw-utils:claw-pointer :void) (register-release-buffer-callback release-callback)))


(defun destory-pixel-buffer (buffer)
  (iffi:destroy-intricate-instance '%filament:backend+pixel-buffer-descriptor buffer))

;;;
;;; SAMPLER
;;;
(u:define-enumval-extractor min-filter-enum %filament:backend+sampler-min-filter)
(u:define-enumval-extractor mag-filter-enum %filament:backend+sampler-mag-filter)
(u:define-enumval-extractor wrap-mode-enum %filament:backend+sampler-wrap-mode)
(u:define-enumval-extractor compare-mode-enum %filament:backend+sampler-compare-mode)
(u:define-enumval-extractor compare-func-enum %filament:backend+sampler-compare-func)


(defun make-sampler (min mag s-wrap r-wrap t-wrap compare-mode compare-func)
  (let ((instance (iffi:make-intricate-instance
                   '%filament:texture-sampler
                   '%filament::texture-sampler+min-filter min
                   '%filament::texture-sampler+mag-filter mag
                   '%filament::texture-sampler+wrap-mode s-wrap
                   '%filament::texture-sampler+wrap-mode r-wrap
                   '%filament::texture-sampler+wrap-mode t-wrap)))
    (%filament:set-compare-mode
     '(claw-utils:claw-pointer %filament::texture-sampler) instance
     '%filament::texture-sampler+compare-mode compare-mode
     '%filament::texture-sampler+compare-func compare-func)
    instance))


(defun destroy-sampler (sampler)
  (iffi:destroy-intricate-instance '%filament:texture-sampler sampler))
