(cl:in-package :%alien-works.skia)


(u:define-enumval-extractor color-type-enum %skia:sk-color-type)
(u:define-enumval-extractor surface-origin-enum %skia:gr-surface-origin)


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


(defun draw-skia (canvas)
  (%skia:clear '(:pointer %skia::sk-canvas) canvas
               '%skia::sk-color %skia:+sk-color-transparent+)
  (let ((time (/ (get-internal-real-time) internal-time-units-per-second)))
    (iffi:with-intricate-instances ((paint %skia:sk-paint))
      (%skia:set-color '(:pointer %skia::sk-paint) paint
                       '%skia::sk-color %skia:+sk-color-yellow+)
      (iffi:with-intricate-alloc (rect %skia:sk-rect)
        (%skia:sk-rect+make-xywh
         '(:pointer %skia::sk-rect) rect
         '%skia::sk-scalar (float (floor (+ 300 (* 100 (cos (* 10 time))))) 0f0)
         '%skia::sk-scalar (float (floor (+ 300 (* 100 (sin (* 10 time))))) 0f0)
         '%skia::sk-scalar 500f0
         '%skia::sk-scalar 500f0)
        (%skia:draw-rect
         '(:pointer %skia::sk-canvas) canvas
         '(:pointer %skia::sk-rect) rect
         '(:pointer %skia::sk-paint) paint))
      (%skia:set-color '(:pointer %skia::sk-paint) paint
                       '%skia::sk-color %skia:+sk-color-cyan+)
      (%skia:draw-circle
       '(:pointer %skia::sk-canvas) canvas
       '%skia::sk-scalar (float (floor (+ 400 (* 150 (sin (* 5 time))))) 0f0)
       '%skia::sk-scalar (float (floor (+ 600 (* 150 (cos (* 1 time))))) 0f0)
       '%skia::sk-scalar 150f0
       '(:pointer %skia::sk-paint) paint)))
  (%skia:flush '(:pointer %skia::sk-canvas) canvas))


(defun call-with-skia-canvas (framebuffer-id width height action)
  (let* ((interface-sp (make-native-gl-interface))
         (context-sp (make-gl-context interface-sp))
         (interface (%skia:get :const '(:pointer %skia::sk-sp<const+gr-gl-interface>) interface-sp))
         (context (%skia:get :const '(:pointer %skia::sk-sp<gr-direct-context>) context-sp)))
    (iffi:with-intricate-instances ((framebuffer %skia:gr-gl-framebuffer-info)
                                    (surface-props %skia:sk-surface-props))
      (iffi:with-intricate-slots %skia:gr-gl-framebuffer-info
          ((fbo-id %skia:f-fboid)
           (format %skia:f-format))
          framebuffer
        (setf fbo-id framebuffer-id
              format #x8058)) ;; #define GR_GL_RGBA8 0x8058
      (iffi:with-intricate-instance
          (render-target %skia:gr-backend-render-target
                         :int width
                         :int height
                         :int 0
                         :int 8
                         '(:pointer %skia::gr-gl-framebuffer-info) framebuffer)
        (let* ((surface-sp (make-surface-from-backend-render-target context render-target surface-props))
               (surface (%skia:get :const '(:pointer %skia::sk-sp<sk-surface>) surface-sp))
               (canvas (%skia:get-canvas '(:pointer %skia::sk-surface) surface)))
          (funcall action canvas))))))
