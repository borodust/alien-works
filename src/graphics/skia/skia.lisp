(cl:in-package :%alien-works.skia)


(declaim (special *canvas*
                  *paint*))

(u:define-enumval-extractor color-type-enum %skia:sk-color-type)
(u:define-enumval-extractor surface-origin-enum %skia:gr-surface-origin)
(u:define-enumval-extractor clip-op-enum %skia:sk-clip-op)


(defun make-surface-from-backend-render-target (context render-target surface-props)
  (iffi:with-intricate-instance (color-space-sp %skia:sk-sp<sk-color-space>)
    (%skia:sk-surface+make-from-backend-render-target
     '(:pointer %skia::sk-sp<sk-surface>) (iffi:intricate-alloc '%skia:sk-sp<sk-surface>)
     '(:pointer %skia::gr-recording-context) context
     '(:pointer %skia::gr-backend-render-target) render-target
     '%skia::gr-surface-origin (surface-origin-enum :bottom-left-gr-surface-origin)
     '%skia::sk-color-type (color-type-enum :rgba-8888-sk-color-type)
     '(:pointer %skia::sk-sp<sk-color-space>) color-space-sp
     '(:pointer %skia::sk-surface-props) surface-props
     '%skia::sk-surface+render-target-release-proc (cffi:null-pointer)
     '%skia::sk-surface+release-context (cffi:null-pointer))))


(defun make-native-gl-interface ()
  (%skia:gr-gl-make-native-interface
   '(:pointer %skia::sk-sp<const+gr-gl-interface>)
   (iffi:intricate-alloc '%skia:sk-sp<const+gr-gl-interface>)))


(defun make-gl-context (gl-interface-sp)
  (%skia:gr-direct-context+make-gl
   '(:pointer %skia::sk-sp<gr-direct-context>) (iffi:intricate-alloc
                                                '%skia:sk-sp<gr-direct-context>)
   '(:pointer %skia::sk-sp<const+gr-gl-interface>) gl-interface-sp))


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
  (%skia:get :const '(:pointer %skia::sk-sp<const+gr-gl-interface>) (%context-interface skia)))


(defun context-handle (skia)
  (%skia:get :const '(:pointer %skia::sk-sp<gr-direct-context>) (%context-handle skia)))


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
                         '(:pointer %skia::gr-gl-framebuffer-info) (%context-framebuffer skia)))
         (surface-sp (iffi:with-intricate-instances ((surface-props %skia:sk-surface-props))
                       (make-surface-from-backend-render-target (context-handle skia)
                                                                render-target
                                                                surface-props)))
         (surface (%skia:get :const '(:pointer %skia::sk-sp<sk-surface>) surface-sp))
         (canvas (%skia:get-canvas '(:pointer %skia::sk-surface) surface)))
    (%make-canvas :surface surface-sp
                  :handle canvas
                  :render-target render-target)))


(defun update-canvas-clip (canvas x y width height &key (mode :intersect))
  (iffi:with-intricate-instance (irect %skia:sk-i-rect)
    (%skia:set-xywh
     '(claw-utils:claw-pointer %skia::sk-i-rect) irect
     '%skia::int32-t (floor x)
     '%skia::int32-t (floor y)
     '%skia::int32-t (floor width)
     '%skia::int32-t (floor height))
    (%skia:clip-i-rect
     '(claw-utils:claw-pointer %skia::sk-canvas) (%canvas-handle canvas)
     '(claw-utils:claw-pointer %skia::sk-i-rect) irect
     '%skia::sk-clip-op mode))
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
  (%skia:clear '(:pointer %skia::sk-canvas) (%canvas-handle canvas)
               '%skia::sk-color %skia:+sk-color-transparent+))


(defun discard-canvas (&optional (canvas *canvas*))
  (%skia:discard '(:pointer %skia::sk-canvas) (%canvas-handle canvas)))


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


(defun rectangle (x y width height)
  (iffi:with-intricate-alloc (rect %skia:sk-rect)
    (%skia:sk-rect+make-xywh
     '(:pointer %skia::sk-rect) rect
     '%skia::sk-scalar (float x 0f0)
     '%skia::sk-scalar (float y 0f0)
     '%skia::sk-scalar (float width 0f0)
     '%skia::sk-scalar (float height 0f0))
    (%skia:draw-rect
     '(:pointer %skia::sk-canvas) (%canvas-handle *canvas*)
     '(:pointer %skia::sk-rect) rect
     '(:pointer %skia::sk-paint) *paint*)))


(defun circle (x y radius)
  (%skia:draw-circle
   '(:pointer %skia::sk-canvas) (%canvas-handle *canvas*)
   '%skia::sk-scalar (float x 0f0)
   '%skia::sk-scalar (float y 0f0)
   '%skia::sk-scalar (float radius 0f0)
   '(:pointer %skia::sk-paint) *paint*))
