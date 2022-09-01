(cl:in-package :%alien-works.tools.filament)


(defun make-image (width height channels)
  (let ((image (iffi:intricate-alloc '%filament.util::image+linear-image)))
    (handler-bind ((serious-condition (lambda (c)
                                        (declare (ignore c))
                                        (iffi:intricate-free image)
                                        (setf image nil))))
      (%filament.util:image+linear-image
       '(claw-utils:claw-pointer %filament.util::image+linear-image) image
       '%filament.util::uint32-t width
       '%filament.util::uint32-t height
       '%filament.util::uint32-t channels)
      image)))


(defun image-width (image)
  (%filament.util:image+get-width
   :const
   '(claw-utils:claw-pointer %filament.util:image+linear-image) image))


(defun image-height (image)
  (%filament.util:image+get-height
   :const
   '(claw-utils:claw-pointer %filament.util:image+linear-image) image))


(defun image-channels (image)
  (%filament.util:image+get-channels
   :const
   '(claw-utils:claw-pointer %filament.util:image+linear-image) image))


(defun destroy-image (image)
  (unwind-protect
       (%filament.util:image+~linear-image
        '(claw-utils:claw-pointer %filament.util:image+linear-image) image)
    (iffi:intricate-free image)))


(defun image-data-ptr (image)
  (%filament.util:image+get-pixel-ref
   :const
   '(claw-utils:claw-pointer %filament.util:image+linear-image) image))


(defun image-data-size (image)
  (* (image-width image)
     (image-height image)
     (image-channels image)
     (iffi:intricate-size :float)))


(defun decode-image (byte-vector &key (color-space :srgb))
  (u:with-pinned-array-pointer (data-ptr byte-vector)
    (let ((image (iffi:intricate-alloc '%filament.util:image+linear-image)))
      (%filament.util:aw+filament+tools+util+decode-image
       '(claw-utils:claw-pointer %filament.util:image+linear-image) image
       '(claw-utils:claw-pointer %filament.util:uint8-t) data-ptr
       '%filament.util:size-t (length byte-vector)
       '%filament.util:image+image-decoder+color-space color-space)
      image)))


(defun encode-image (image byte-vector &key (format :png))
  (u:with-pinned-array-pointer (data-ptr byte-vector)
    (let ((written (%filament.util:aw+filament+tools+util+encode-image
                    '(claw-utils:claw-pointer %filament.util:uint8-t) data-ptr
                    '%filament.util:size-t (length byte-vector)
                    '%filament.util:image+image-encoder+format format
                    '(claw-utils:claw-pointer %filament.util:image+linear-image) image)))
      (when (> written 0)
        written))))



(%aw.fm:warp-intricate-builder-option compressed-texture-encoder-builder :linear
    %filament.util:image+linear
  '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder)
  :bool)


(%aw.fm:warp-intricate-builder-option compressed-texture-encoder-builder :cubemap
    %filament.util:image+cubemap
  '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder)
  :bool)


(%aw.fm:warp-intricate-builder-option compressed-texture-encoder-builder :intermediate-format
    %filament.util:image+intermediate-format
  '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder)
  '%filament.util:image+basis-encoder+intermediate-format)


(%aw.fm:warp-intricate-builder-option compressed-texture-encoder-builder :grayscale
    %filament.util:image+grayscale
  '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder)
  :bool)


(%aw.fm:warp-intricate-builder-option compressed-texture-encoder-builder :normals
    %filament.util:image+normals
  '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder)
  :bool)


(%aw.fm:warp-intricate-builder-option compressed-texture-encoder-builder :jobs
    %filament.util:image+jobs
  '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder)
  '%filament.util:size-t)


(%aw.fm:warp-intricate-builder-option compressed-texture-encoder-builder :quiet
    %filament.util:image+quiet
  '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder)
  :bool)


(%aw.fm:warp-intricate-builder-option compressed-texture-encoder-builder :mip-level
    %filament.util:image+miplevel
  '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder)
  '%filament.util:size-t
  '%filament.util:size-t
  '(claw-utils:claw-pointer %filament.util:image+linear-image))


(defmacro with-compressed-texture-encoder-builder (((name mip-count layer-count) &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament.util:image+basis-encoder+builder
             '%filament.util:size-t ,mip-count
             '%filament.util:size-t ,layer-count))
         (build-expander (builder)
           `(%filament.util:image+build
             '(claw-utils:claw-pointer %filament.util:image+basis-encoder+builder) ,builder)))
    (%aw.fm:explode-builder name
                            'compressed-texture-encoder-builder
                            #'ctor-expander
                            #'build-expander
                            '()
                            ;; we hardcode number of jobs
                            ;; making encoding single-threaded
                            ;; due to the SBCL problems with SIGFPE
                            (append steps
                                    '((:jobs 1)))
                            body)))


(defun encode-compressed-texture (encoder)
  (u:without-float-traps
    (when (%filament.util:image+encode
           '(claw-utils:claw-pointer %filament.util:image+basis-encoder) encoder)
      (values (%filament.util:image+get-ktx2data
               :const
               '(claw-utils:claw-pointer %filament.util:image+basis-encoder) encoder)
              (%filament.util:image+get-ktx2byte-count
               :const
               '(claw-utils:claw-pointer %filament.util:image+basis-encoder) encoder)))))
