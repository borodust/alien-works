(cl:in-package :%alien-works.graphics)


(u:define-enumval-extractor texture-sampler-type-enum %filament:filament+backend+sampler-type)
(u:define-enumval-extractor texture-internal-format-enum %filament:filament+texture+internal-format)
(u:define-enumval-extractor texture-cubemap-face-enum %filament:filament+texture+cubemap-face)
(u:define-enumval-extractor texture-usage-enum %filament:filament+texture+usage)
(u:define-enumval-extractor texture-swizzle-enum %filament:filament+texture+swizzle)

(u:define-enumval-extractor pixel-format-enum %filament:filament+backend+pixel-data-format)
(u:define-enumval-extractor pixel-type-enum %filament:filament+backend+pixel-data-type)
(u:define-enumval-extractor pixel-compressed-type-enum
  %filament:filament+backend+compressed-pixel-data-type)


(warp-intricate-builder-option texture-builder :width
    %filament:filament+width
  '(:pointer %filament:filament+texture+builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :height
    %filament:filament+height
  '(:pointer %filament:filament+texture+builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :depth
    %filament:filament+depth
  '(:pointer %filament:filament+texture+builder)
  '%filament:uint32-t)

(warp-intricate-builder-option texture-builder :levels
    %filament:filament+levels
  '(:pointer %filament:filament+texture+builder)
  '%filament:uint8-t)

(warp-intricate-builder-option texture-builder :sampler
    %filament:filament+sampler
  '(:pointer %filament:filament+texture+builder)
  '%filament:filament+texture+sampler)

(warp-intricate-builder-option texture-builder :format
    %filament:filament+format
  '(:pointer %filament:filament+texture+builder)
  '%filament:filament+texture+internal-format)

(warp-intricate-builder-option texture-builder :usage
    %filament:filament+usage
  '(:pointer %filament:filament+texture+builder)
  '%filament:filament+texture+usage)

(warp-intricate-builder-option texture-builder :swizzle
    %filament:filament+swizzle
  '(:pointer %filament:filament+texture+builder)
  '%filament:filament+texture+swizzle
  '%filament:filament+texture+swizzle
  '%filament:filament+texture+swizzle
  '%filament:filament+texture+swizzle)

(warp-intricate-builder-option texture-builder :import
    %filament:filament+import
  '(:pointer %filament:filament+texture+builder)
  '%filament:intptr-t)


(defmacro with-texture-builder ((name &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:filament+texture+builder))
         (build-expander (builder)
           `(%filament:filament+build
             '(:pointer %filament:filament+texture+builder) ,builder
             '(:pointer %filament:filament+engine) !::engine)))
    (explode-builder name
                     'texture-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-texture (engine texture)
  (%filament:filament+destroy
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::filament+texture) texture))


(defun update-texture-image (engine texture level pixel-buffer)
  (%filament:filament+set-image
   '(:pointer %filament:filament+texture) texture
   '(:pointer %filament:filament+engine) engine
   '%filament:size-t level
   '(:pointer %filament:filament+texture+pixel-buffer-descriptor) pixel-buffer))


(defun update-texture-subimage (engine texture level x-offset y-offset width height pixel-buffer)
  (%filament:filament+set-image
   '(:pointer %filament:filament+texture) texture
   '(:pointer %filament:filament+engine) engine
   '%filament:size-t level
   '%filament:uint32-t x-offset
   '%filament:uint32-t y-offset
   '%filament:uint32-t width
   '%filament:uint32-t height
   '(:pointer %filament:filament+texture+pixel-buffer-descriptor) pixel-buffer))


(defun update-texture-array-subimage (engine texture level x-offset y-offset z-offset
                                      width height depth
                                      pixel-buffer)
  (%filament:filament+set-image
   '(:pointer %filament:filament+texture) texture
   '(:pointer %filament:filament+engine) engine
   '%filament:size-t level
   '%filament:uint32-t x-offset
   '%filament:uint32-t y-offset
   '%filament:uint32-t z-offset
   '%filament:uint32-t width
   '%filament:uint32-t height
   '%filament:uint32-t depth
   '(:pointer %filament:filament+texture+pixel-buffer-descriptor) pixel-buffer))


(defun update-cubemap-images (engine texture level pixel-buffer face-offsets)
  (%filament:filament+set-image
   '(:pointer %filament:filament+texture) texture
   '(:pointer %filament:filament+engine) engine
   '%filament:size-t level
   '(:pointer %filament:filament+texture+pixel-buffer-descriptor) pixel-buffer
   '(:pointer %filament:filament+texture+face-offsets) face-offsets))


(defun generate-texture-mipmaps (engine texture)
  (%filament:filament+generate-mipmaps
   '(:pointer %filament:filament+texture) texture
   '(:pointer %filament:filament+engine) engine))


(defun generate-texture-prefilter-mipmaps (engine texture pixel-buffer face-offsets
                                           &optional prefilter-options)
  (%filament:filament+generate-prefilter-mipmap
   '(:pointer %filament:filament+texture) texture
   '(:pointer %filament:filament+engine) engine
   '(:pointer %filament:filament+texture+pixel-buffer-descriptor) pixel-buffer
   '(:pointer %filament:filament+texture+face-offsets) face-offsets
   '(:pointer %filament:filament+texture+prefilter-options) prefilter-options))


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


(cffi:defcallback release-buffer-callback :pointer ((data :pointer)
                                                    (size %filament:size-t)
                                                    (user :pointer))
  (declare (ignore data size))
  (a:when-let (callback (gethash (cffi:pointer-address user) *buffer-release-callback-table*))
    (unwind-protect
         (funcall callback)
      (cffi:foreign-free user)
      (remhash (cffi:pointer-address user) *buffer-release-callback-table*)))
  (cffi:null-pointer))


(defun make-pixel-buffer (data-ptr data-size pixel-format pixel-type &optional release-callback)
  (iffi:make-intricate-instance
   '%filament:filament+backend+pixel-buffer-descriptor
   '(:pointer :void) data-ptr
   '%filament:size-t data-size
   '%filament:filament+backend+pixel-buffer-descriptor+pixel-data-format pixel-format
   '%filament:filament+backend+pixel-buffer-descriptor+pixel-data-type pixel-type
   '%filament:filament+backend+buffer-descriptor+callback (cffi:callback release-buffer-callback)
   '(:pointer :void) (register-release-buffer-callback release-callback)))


(defun make-compressed-pixel-buffer (data-ptr data-size compressed-size compressed-pixel-type
                                     &optional release-callback)
  (iffi:make-intricate-instance
   '%filament:filament+backend+pixel-buffer-descriptor
   '(:pointer :void) data-ptr
   '%filament:size-t data-size
   '%filament:filament+backend+compressed-pixel-data-type compressed-pixel-type
   '%filament:uint32-t compressed-size
   '%filament:filament+backend+buffer-descriptor+callback (cffi:callback release-buffer-callback)
   '(:pointer :void) (register-release-buffer-callback release-callback)))


(defun destory-pixel-buffer (buffer)
  (iffi:destroy-intricate-instance '%filament:filament+backend+pixel-buffer-descriptor buffer))

;;;
;;; SAMPLER
;;;
(u:define-enumval-extractor min-filter-enum %filament:filament+backend+sampler-min-filter)
(u:define-enumval-extractor mag-filter-enum %filament:filament+backend+sampler-mag-filter)
(u:define-enumval-extractor wrap-mode-enum %filament:filament+backend+sampler-wrap-mode)
(u:define-enumval-extractor compare-mode-enum %filament:filament+backend+sampler-compare-mode)
(u:define-enumval-extractor compare-func-enum %filament:filament+backend+sampler-compare-func)


(defun make-sampler (min mag s-wrap r-wrap t-wrap compare-mode compare-func)
  (let ((instance (iffi:make-intricate-instance
                   '%filament:filament+texture-sampler
                   '%filament::filament+texture-sampler+min-filter min
                   '%filament::filament+texture-sampler+mag-filter mag
                   '%filament::filament+texture-sampler+wrap-mode s-wrap
                   '%filament::filament+texture-sampler+wrap-mode r-wrap
                   '%filament::filament+texture-sampler+wrap-mode t-wrap)))
    (%filament:filament+set-compare-mode
     '(:pointer %filament::filament+texture-sampler) instance
     '%filament::filament+texture-sampler+compare-mode compare-mode
     '%filament::filament+texture-sampler+compare-func compare-func)
    instance))


(defun destroy-sampler (sampler)
  (iffi:destroy-intricate-instance '%filament:filament+texture-sampler sampler))
