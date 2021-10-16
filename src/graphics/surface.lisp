(cl:in-package :alien-works.graphics)


(atomics:defstruct (triple-buffered-value
                    (:constructor %make-triple-buffered-value))
  (updating-p nil :type boolean)
  (prepared-p nil :type boolean)
  (front nil :type t)
  (prepared nil :type t)
  (back nil :type t))


(defun make-triple-buffered-value (front prepared back)
  (%make-triple-buffered-value :front front :prepared prepared :back back))


(defun lock-triple-buffered-value (vessel)
  (loop until (atomics:cas (triple-buffered-value-updating-p vessel) nil t)))


(defun unlock-triple-buffered-value (vessel)
  (setf (triple-buffered-value-updating-p vessel) nil))


(defmacro with-locked-triple-buffered-value ((vessel) &body body)
  (a:once-only (vessel)
    `(progn
       (lock-triple-buffered-value ,vessel)
       (unwind-protect
            (progn ,@body)
         (unlock-triple-buffered-value ,vessel)))))


(defun swap-triple-buffered-value (vessel)
  (with-locked-triple-buffered-value (vessel)
    (when (triple-buffered-value-prepared-p vessel)
      (rotatef (triple-buffered-value-front vessel)
               (triple-buffered-value-prepared vessel))
      (setf (triple-buffered-value-prepared-p vessel) nil))
    (triple-buffered-value-front vessel)))


(defun prepare-triple-buffered-value (vessel)
  (with-locked-triple-buffered-value (vessel)
    (rotatef (triple-buffered-value-back vessel)
             (triple-buffered-value-prepared vessel))
    (setf (triple-buffered-value-prepared-p vessel) t)))


;;;
;;;
;;;
(defstruct (buffered-surface-framebuffer
            (:constructor %make-buffered-surface-framebuffer))
  (id -1 :type fixnum :read-only t)
  (depth-stencil-buffer-id -1 :type fixnum :read-only t))


(defun make-buffered-surface-framebuffer (width height)
  (let ((fbo (gl:gen-framebuffer))
        (rbo (gl:gen-renderbuffer)))
    (gl:bind-framebuffer :framebuffer fbo)
    (gl:bind-renderbuffer :renderbuffer rbo)
    (gl:renderbuffer-storage :renderbuffer :depth24-stencil8 width height)
    (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment :renderbuffer rbo)
    (%make-buffered-surface-framebuffer :id fbo
                                        :depth-stencil-buffer-id rbo)))


(defun destroy-buffered-surface-framebuffer (instance)
  (gl:delete-renderbuffers (list (buffered-surface-framebuffer-depth-stencil-buffer-id instance)))
  (gl:delete-framebuffers (list (buffered-surface-framebuffer-id instance))))


(defun prepare-framebuffer (framebuffer texture-id viewport-width viewport-height)
  (gl:bind-framebuffer :framebuffer (buffered-surface-framebuffer-id framebuffer))
  ;; FIXME: attach texture only when needed
  (%gl:framebuffer-texture :framebuffer :color-attachment0 texture-id 0)
  (%gl:viewport 0 0 viewport-width viewport-height)
  (gl:clear-color 0f0 0f0 0f0 0f0)
  (gl:clear :color-buffer :depth-buffer :stencil-buffer))


(defun flush-framebuffer (framebuffer)
  (gl:bind-framebuffer :framebuffer (buffered-surface-framebuffer-id framebuffer))
  ;; FIXME: detach texture only when needed
  (%gl:framebuffer-texture :framebuffer :color-attachment0 0 0))

;;;
;;;
;;;
(defstruct (buffered-surface-texture
            (:constructor %make-buffered-surface-texture))
  (id -1 :type fixnum :read-only t)
  (filatex nil :read-only t))


(defun make-buffered-surface-texture (engine width height)
  (let ((tex-id (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex-id)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0
                     :rgba :unsigned-byte (cffi:null-pointer) :raw t)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-base-level 0)
    (gl:tex-parameter :texture-2d :texture-max-level 0)
    (%make-buffered-surface-texture
     :id tex-id
     :filatex (make-texture engine
                            (.import tex-id)
                            (.sampler :2d)
                            (.width width)
                            (.height height)
                            (.format :rgba8)
                            (.levels 1)))))


(defun destroy-buffered-surface-texture (engine instance)
  (destroy-texture engine (buffered-surface-texture-filatex instance))
  (gl:delete-texture (buffered-surface-texture-id instance)))


(atomics:defstruct (buffered-surface
                    (:constructor %make-buffered-surface))
  (width 1 :type fixnum :read-only t)
  (height 1 :type fixnum :read-only t)
  (prepared-p nil :type boolean)
  (front -1 :type (or buffered-surface-texture null))
  (prepared -1 :type (or buffered-surface-texture null))
  (back -1 :type (or buffered-surface-texture null))
  (updating-p nil))


(defun make-buffered-surface (engine width height)
  (flet ((%make-texture ()
           (make-buffered-surface-texture engine width height)))
    (%make-buffered-surface
     :width width
     :height height
     :front (%make-texture)
     :prepared (%make-texture)
     :back (%make-texture))))


(defun destroy-buffered-surface (engine instance)
  (destroy-buffered-surface-texture engine (buffered-surface-front instance))
  (destroy-buffered-surface-texture engine (buffered-surface-back instance))
  (destroy-buffered-surface-texture engine (buffered-surface-prepared instance)))


(defun lock-buffered-surface (buffered-surface)
  (loop until (atomics:cas (buffered-surface-updating-p buffered-surface) nil t)))


(defun unlock-buffered-surface (buffered-surface)
  (setf (buffered-surface-updating-p buffered-surface) nil))


(defmacro with-locked-buffered-surface ((surface) &body body)
  (a:once-only (surface)
    `(progn
       (lock-buffered-surface ,surface)
       (unwind-protect
            (progn ,@body)
         (unlock-buffered-surface ,surface)))))


(defun buffered-surface-acquire (buffered-surface)
  (buffered-surface-texture-filatex (buffered-surface-front buffered-surface)))


(defun buffered-surface-release (buffered-surface)
  (with-locked-buffered-surface (buffered-surface)
    (when (buffered-surface-prepared-p buffered-surface)
      (rotatef (buffered-surface-front buffered-surface)
               (buffered-surface-prepared buffered-surface))
      (setf (buffered-surface-prepared-p buffered-surface) nil))))


(defun swap-buffered-surface (buffered-surface)
  (with-locked-buffered-surface (buffered-surface)
    (rotatef (buffered-surface-back buffered-surface)
             (buffered-surface-prepared buffered-surface))
    (setf (buffered-surface-prepared-p buffered-surface) t)))
