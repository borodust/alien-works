(cl:in-package :%alien-works.skia)


(declaim (special *canvas*
                  *paint*
                  *font*))

(u:define-enumval-extractor color-type-enum %skia:sk-color-type)
(u:define-enumval-extractor surface-origin-enum %skia:gr-surface-origin)
(u:define-enumval-extractor clip-op-enum %skia:sk-clip-op)


(defun make-surface-from-backend-render-target (context render-target surface-props)
  (iffi:with-intricate-instance (color-space-sp %skia:sk-sp<sk-color-space>)
    (%skia:sk-surface+make-from-backend-render-target
     '(:pointer %skia:sk-sp<sk-surface>) (iffi:intricate-alloc '%skia:sk-sp<sk-surface>)
     '(:pointer %skia:gr-recording-context) context
     '(:pointer %skia:gr-backend-render-target) render-target
     '%skia:gr-surface-origin (surface-origin-enum :bottom-left-gr-surface-origin)
     '%skia:sk-color-type (color-type-enum :rgba-8888-sk-color-type)
     '(:pointer %skia:sk-sp<sk-color-space>) color-space-sp
     '(:pointer %skia:sk-surface-props) surface-props
     '%skia:sk-surface+render-target-release-proc (cffi:null-pointer)
     '%skia:sk-surface+release-context (cffi:null-pointer))))


(defun make-native-gl-interface ()
  (%skia:gr-gl-make-native-interface
   '(:pointer %skia:sk-sp<const+gr-gl-interface>)
   (iffi:intricate-alloc '%skia:sk-sp<const+gr-gl-interface>)))


(defun make-gl-context (gl-interface-sp)
  (%skia:gr-direct-context+make-gl
   '(:pointer %skia:sk-sp<gr-direct-context>) (iffi:intricate-alloc
                                                '%skia:sk-sp<gr-direct-context>)
   '(:pointer %skia:sk-sp<const+gr-gl-interface>) gl-interface-sp))


;;;
;;; SKIA
;;;
(defstruct (skia-context
            (:constructor %make-context)
            (:conc-name %context-))
  interface
  handle
  framebuffer)


(defun context-interface (skia)
  (%skia:get :const '(:pointer %skia:sk-sp<const+gr-gl-interface>) (%context-interface skia)))


(defun context-handle (skia)
  (%skia:get :const '(:pointer %skia:sk-sp<gr-direct-context>) (%context-handle skia)))


(defun make-context (framebuffer-id)
  (let* ((interface-sp (make-native-gl-interface))
         (context-sp (make-gl-context interface-sp))
         (framebuffer (iffi:make-intricate-instance '%skia:gr-gl-framebuffer-info)))
    (iffi:with-intricate-slots %skia:gr-gl-framebuffer-info
        ((fbo-id %skia:f-fboid)
         (format %skia:f-format))
        framebuffer
      (setf fbo-id framebuffer-id
            format #x8058)) ;; #define GR_GL_RGBA8 0x8058

    (%make-context :interface interface-sp
                   :handle context-sp
                   :framebuffer framebuffer)))


(defun flush-context (skia)
  (%skia:flush
   '(claw-utils:claw-pointer %skia:gr-direct-context) (context-handle skia)))


(defun destroy-context (skia)
  ;; FIXME: do things
  )



;;;
;;; CANVAS
;;;
(defstruct (skia-canvas
            (:constructor %make-canvas)
            (:conc-name %canvas-))
  render-target
  surface
  handle)


(defun make-canvas (skia width height)
  (let* ((render-target (iffi:make-intricate-instance
                         '%skia:gr-backend-render-target
                         :int width
                         :int height
                         :int 0
                         :int 8
                         '(:pointer %skia:gr-gl-framebuffer-info) (%context-framebuffer skia)))
         (surface-sp (iffi:with-intricate-instances ((surface-props %skia:sk-surface-props))
                       (make-surface-from-backend-render-target (context-handle skia)
                                                                render-target
                                                                surface-props)))
         (surface (%skia:get :const '(:pointer %skia:sk-sp<sk-surface>) surface-sp))
         (canvas (%skia:get-canvas '(:pointer %skia:sk-surface) surface)))
    (%make-canvas :surface surface-sp
                  :handle canvas
                  :render-target render-target)))


(defun update-canvas-clip (canvas x y width height &key (mode :intersect))
  (iffi:with-intricate-instance (irect %skia:sk-i-rect)
    (%skia:set-xywh
     '(claw-utils:claw-pointer %skia:sk-i-rect) irect
     '%skia:int32-t (floor x)
     '%skia:int32-t (floor y)
     '%skia:int32-t (floor width)
     '%skia:int32-t (floor height))
    (%skia:clip-i-rect
     '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle canvas)
     '(claw-utils:claw-pointer %skia:sk-i-rect) irect
     '%skia:sk-clip-op mode))
  canvas)


(defun destroy-canvas (canvas)
  ;; FIXME: do it
  )


;;;
;;; DRAWING
;;;
(defun make-paint ()
  (iffi:make-intricate-instance '%skia:sk-paint))


(defun destroy-paint (paint)
  (iffi:destroy-intricate-instance '%skia:sk-paint paint))


(defun clear-canvas (&optional (canvas *canvas*))
  (%skia:clear '(:pointer %skia:sk-canvas) (%canvas-handle canvas)
               '%skia:sk-color %skia:+sk-color-transparent+))


(defun discard-canvas (&optional (canvas *canvas*))
  (%skia:discard '(:pointer %skia:sk-canvas) (%canvas-handle canvas)))


(defun flush-canvas (&optional (canvas *canvas*))
  (%skia:flush '(:pointer %skia:sk-canvas) (%canvas-handle canvas)))


(defun paint-color (r g b a)
  (iffi:with-intricate-instance (color %skia:sk-color4f)
    (iffi:with-intricate-slots %skia:sk-color4f ((cr %skia:f-r)
                                                 (cg %skia:f-g)
                                                 (cb %skia:f-b)
                                                 (ca %skia:f-a))
                               color
      (setf cr (float r 0f0)
            cg (float g 0f0)
            cb (float b 0f0)
            ca (float a 0f0))
      (%skia:set-color
       '(claw-utils:claw-pointer %skia:sk-paint) *paint*
       '(claw-utils:claw-pointer %skia:sk-color4f) color
       '(claw-utils:claw-pointer %skia:sk-color-space) (cffi:null-pointer)))))


(defun font-size (size)
  (when *font*
    (%skia:set-size
     '(claw-utils:claw-pointer %skia:sk-font) *font*
     '%skia:sk-scalar (float size 0f0))))


(defun font-baseline-snap (snapped)
  (when *font*
    (%skia:set-baseline-snap
     '(claw-utils:claw-pointer %skia:sk-font) *font*
     :bool (and snapped t))))


(defun font-edging (mode)
  (when *font*
    (%skia:set-edging
     '(claw-utils:claw-pointer %skia:sk-font) *font*
     '%skia::sk-font+edging mode)))


(defun font-subpixel (subpixeled)
  (when *font*
    (%skia:set-subpixel
     '(claw-utils:claw-pointer %skia:sk-font) *font*
     :bool (and subpixeled t))))


(defun rectangle (x y width height)
  (iffi:with-intricate-alloc (rect %skia:sk-rect)
    (%skia:sk-rect+make-xywh
     '(:pointer %skia:sk-rect) rect
     '%skia:sk-scalar (float x 0f0)
     '%skia:sk-scalar (float y 0f0)
     '%skia:sk-scalar (float width 0f0)
     '%skia:sk-scalar (float height 0f0))
    (%skia:draw-rect
     '(:pointer %skia:sk-canvas) (%canvas-handle *canvas*)
     '(:pointer %skia:sk-rect) rect
     '(:pointer %skia:sk-paint) *paint*)))


(defun circle (x y radius)
  (%skia:draw-circle
   '(:pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)
   '%skia:sk-scalar (float radius 0f0)
   '(:pointer %skia:sk-paint) *paint*))


(defun text (text x y)
  (when *font*
    (cffi:with-foreign-string ((ftext byte-size) text :encoding :utf-8)
      (%skia:draw-simple-text
       '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
       '(claw-utils:claw-pointer :void) ftext
       '%skia:size-t (1- byte-size)
       '%skia:sk-text-encoding :utf8
       '%skia:sk-scalar (float x 0f0)
       '%skia:sk-scalar (float y 0f0)
       '(claw-utils:claw-pointer %skia:sk-font) *font*
       '(claw-utils:claw-pointer %skia:sk-paint) *paint*))))


(defun save-transform ()
  (%skia:save '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)))


(defun restore-transform ()
  (%skia:restore '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)))


(defun reset-transform ()
  (%skia:restore-to-count
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   :int 1))


(defun translate (x y)
  (%skia:translate
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))


(defun rotate (degrees)
  (%skia:rotate
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float degrees 0f0)))


(defun rotate-around (x y degrees)
  (%skia:rotate
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float degrees 0f0)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))


(defun scale (x y)
  (%skia:scale
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))

;;;
;;; FONTS
;;;
(defun make-typeface (font-data-ub8-array)
  (assert (or (subtypep (array-element-type font-data-ub8-array) '(unsigned-byte 8))
              (subtypep (array-element-type font-data-ub8-array) '(signed-byte 8))))
  (let ((typeface (iffi:intricate-alloc '%skia:sk-sp<sk-typeface>)))
    (u:with-pinned-array-pointer (font-data-ptr font-data-ub8-array)
      (iffi:with-intricate-alloc (data %skia:sk-sp<sk-data>)
        (%skia:sk-data+make-with-copy
         '(claw-utils:claw-pointer %skia:sk-sp<sk-data>) data
         '(claw-utils:claw-pointer :void) font-data-ptr
         '%skia:size-t (length font-data-ub8-array))
        (unwind-protect
             (progn
               (%skia:sk-typeface+make-from-data
                '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface
                '(claw-utils:claw-pointer %skia:sk-sp<sk-data>) data
                :int 0))
          (%skia:~sk-sp
           '(claw-utils:claw-pointer %skia:sk-sp<sk-data>) data))))
    typeface))


(defun destroy-typeface (typeface)
  (%skia:~sk-sp
   '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface)
  (iffi:intricate-free typeface)
  (values))


(defun %typeface-family-name (typeface-ptr)
  (unless (cffi:null-pointer-p typeface-ptr)
    (iffi:with-intricate-instance (sk-str %skia:sk-string)
      (%skia:get-family-name
       :const
       '(claw-utils:claw-pointer %skia:sk-typeface) typeface-ptr
       '(claw-utils:claw-pointer %skia:sk-string) sk-str)
      (cffi:foreign-string-to-lisp
       (%skia:c-str :const '(claw-utils:claw-pointer %skia:sk-string) sk-str)
       :encoding :utf-8))))


(defun typeface-family-name (typeface)
  (%typeface-family-name (%skia:get
                          :const
                          '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface)))


(defun make-default-font ()
  (iffi:make-intricate-instance '%skia:sk-font))


(defun make-font (typeface)
  (iffi:make-intricate-instance '%skia:sk-font
                                '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface))


(defun destroy-font (font)
  (iffi:destroy-intricate-instance '%skia:sk-font font))


(defun font-family-name (font)
  (%typeface-family-name
   (%skia:get-typeface-or-default :const '(claw-utils:claw-pointer %skia:sk-font) font)))
