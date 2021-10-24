(cl:in-package :alien-works.graphics)


(declaim (special *canvas*))

(defvar *paint-stack* (make-array 1 :fill-pointer 0 :adjustable t))
(defvar *font-stack* (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter *pi-degree* (float (/ 180 pi) 0f0))

;;;
;;; PAINT
;;;
(defun %push-paint ()
  (let ((paint (%aw.skia:make-paint)))
    (vector-push-extend paint *paint-stack*)
    (setf %aw.skia:*paint* paint)))


(defun %pop-paint ()
  (%aw.skia:destroy-paint (vector-pop *paint-stack*))
  (setf %aw.skia:*paint* (when (> (length *paint-stack*) 0)
                           (aref *paint-stack* (1- (length *paint-stack*))))))


;;;
;;; FONT
;;;
(defun %push-font (&optional typeface)
  (let ((font (if typeface
                  (%aw.skia:make-font typeface)
                  (%aw.skia:make-default-font))))
    (vector-push-extend font *font-stack*)
    (setf %aw.skia:*font* font)))


(defun %pop-font ()
  (%aw.skia:destroy-font (vector-pop *font-stack*))
  (setf %aw.skia:*font* (when (> (length *font-stack*) 0)
                          (aref *font-stack* (1- (length *font-stack*))))))


;;;
;;; FRAMEBUFFER
;;;
(defstruct (canvas-context-framebuffer
            (:constructor %make-canvas-context-framebuffer))
  (id -1 :type fixnum :read-only t))


(defun make-canvas-context-framebuffer ()
  (let ((fbo (gl:gen-framebuffer)))
    (gl:bind-framebuffer :framebuffer fbo)
    (%make-canvas-context-framebuffer :id fbo)))


(defun destroy-canvas-context-framebuffer (instance)
  (gl:delete-framebuffers (list (canvas-context-framebuffer-id instance))))


(defun bind-framebuffer (framebuffer renderbuffer-id texture-id viewport-width viewport-height)
  (gl:bind-framebuffer :framebuffer (canvas-context-framebuffer-id framebuffer))

  (gl:bind-renderbuffer :renderbuffer renderbuffer-id)
  (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment :renderbuffer renderbuffer-id)

  (%gl:framebuffer-texture :framebuffer :color-attachment0 texture-id 0)
  (%gl:viewport 0 0 viewport-width viewport-height)
  (gl:clear-color 0f0 0f0 0f0 0f0)
  (gl:clear :color-buffer :depth-buffer :stencil-buffer))


(defun flush-framebuffer (framebuffer)
  (gl:bind-framebuffer :framebuffer (canvas-context-framebuffer-id framebuffer))
  (%gl:framebuffer-texture :framebuffer :color-attachment0 0 0)
  (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment :renderbuffer 0))


;;;
;;; CAS-QUEUE
;;;
(atomics:defstruct (cas-queue)
  (locked-p nil :type boolean)
  (list (list) :type list))


(defun lock-cas-queue (queue)
  (loop until (atomics:cas (cas-queue-locked-p queue) nil t)))


(defun unlock-cas-queue (queue)
  (setf (cas-queue-locked-p queue) nil))


(defmacro with-locked-cas-queue ((queue) &body body)
  (a:once-only (queue)
    `(progn
       (lock-cas-queue ,queue)
       (unwind-protect
            (progn ,@body)
         (unlock-cas-queue ,queue)))))


(defun push-cas-queue-value (queue value)
  (with-locked-cas-queue (queue)
    (push value (cas-queue-list queue))))


(defun dump-cas-queue-values (queue)
  (with-locked-cas-queue (queue)
    (prog1 (cas-queue-list queue)
      (setf (cas-queue-list queue) (list)))))


;;;
;;; UNBUFFERED TEXTURE
;;;
(defstruct (unbuffered-texture
            (:constructor %make-unbuffered-texture)
            (:conc-name %unbuffered-texture-))
  id
  filatex)


(defun make-unbuffered-texture (engine width height)
  (let ((tex-id (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex-id)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0
                     :rgba :unsigned-byte (cffi:null-pointer) :raw t)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-base-level 0)
    (gl:tex-parameter :texture-2d :texture-max-level 0)
    (%make-unbuffered-texture
     :id tex-id
     :filatex (make-texture engine
                            (.import tex-id)
                            (.sampler :2d)
                            (.width width)
                            (.height height)
                            (.format :rgba8)
                            (.levels 1)))))

(defun unbuffered-texture-id (unbuffered-texture)
  (%unbuffered-texture-id unbuffered-texture))


(defun unbuffered-texture-handle (unbuffered-texture)
  (%unbuffered-texture-filatex unbuffered-texture))


(defun destroy-unbuffered-texture (engine unbuffered-texture)
  (destroy-texture engine (%unbuffered-texture-filatex unbuffered-texture))
  (gl:delete-texture (%unbuffered-texture-id unbuffered-texture)))


;;;
;;; DRAW COMMAND
;;;
(atomics:defstruct (draw-command
                    (:constructor make-draw-command ())
                    (:conc-name %draw-command-))
  (name nil)
  (arguments nil))


(defun update-draw-command (command name &rest arguments)
  (setf (%draw-command-name command) name
        (%draw-command-arguments command) arguments))


(defun invoke-draw-command (command)
  (apply (%draw-command-name command) (%draw-command-arguments command)))

;;;
;;; COMMAND QUEUE
;;;
(atomics:defstruct (canvas-command-queue
                    (:constructor make-canvas-command-queue ())
                    (:conc-name %canvas-command-queue-))
  (array (make-array 128 :fill-pointer 0 :adjustable t :initial-element nil)))


(defun push-command (queue name &rest args)
  (let* ((queue (%canvas-command-queue-array queue))
         (fp (fill-pointer queue)))
    (if (= fp (array-total-size queue))
        (vector-push-extend nil queue 128)
        (setf (fill-pointer queue) (1+ fp)))

    (let ((next-command (a:if-let ((next-command (aref queue fp)))
                          next-command
                          (setf (aref queue fp) (make-draw-command)))))
      (apply #'update-draw-command next-command name args)))
  (values))


(defun drain-draw-commands (queue)
  (let ((array (%canvas-command-queue-array queue)))
    (unwind-protect
         (loop for command across array
               do (invoke-draw-command command)
                  (update-draw-command command nil))
      (setf (fill-pointer array) 0))))


(defun discard-command-queue (queue)
  (setf (fill-pointer (%canvas-command-queue-array queue)) 0)
  (values))

;;;
;;; CANVAS
;;;
(atomics:defstruct (canvas
                    (:constructor %make-canvas)
                    (:conc-name %canvas-))
  engine
  context
  handle

  width
  height
  depth-stencil-renderbuffer-id
  last-flushed
  triple-command-queue
  triple-texture)


(defun make-canvas (engine width height)
  (let ((canvas-context (canvas-context-of engine)))
    (flet ((%unbufered-texture ()
             (make-unbuffered-texture engine width height)))
      (let ((canvas (%make-canvas :triple-command-queue (make-triple-buffered-value
                                                         (make-canvas-command-queue)
                                                         (make-canvas-command-queue)
                                                         (make-canvas-command-queue))
                                  :triple-texture (make-triple-buffered-value (%unbufered-texture)
                                                                              (%unbufered-texture)
                                                                              (%unbufered-texture))
                                  :width width
                                  :height height
                                  :handle (%aw.skia:make-canvas
                                           (canvas-context-handle canvas-context) width height)
                                  :context canvas-context
                                  :engine engine)))
        (flet ((%init ()
                 (let ((rbo (gl:gen-renderbuffer)))
                   (gl:bind-renderbuffer :renderbuffer rbo)
                   (gl:renderbuffer-storage :renderbuffer :depth24-stencil8 width height)
                   (setf (%canvas-depth-stencil-renderbuffer-id canvas) rbo)
                   (push-canvas-context-canvas canvas-context canvas))))
          (push-canvas-context-task canvas-context #'%init))
        canvas))))


(defun destroy-canvas (canvas)
  (let ((engine (%canvas-engine canvas)))
    (flet ((%destroy ()
             (delete-canvas-context-canvas (%canvas-context canvas) canvas)
             (gl:delete-renderbuffers (list (%canvas-depth-stencil-renderbuffer-id canvas)))
             (%aw.skia:destroy-canvas (%canvas-handle canvas))
             (flet ((~texture (unbuffered-texture)
                      (destroy-unbuffered-texture engine unbuffered-texture)))
               (let ((tritex (%canvas-triple-texture canvas)))
                 (~texture (triple-buffered-value-front tritex))
                 (~texture (triple-buffered-value-prepared tritex))
                 (~texture (triple-buffered-value-back tritex))))))
      (push-canvas-context-task (%canvas-context canvas) #'%destroy))))


(defun draw-canvas (canvas framebuffer)
  (let ((front-command-queue (swap-triple-buffered-value (%canvas-triple-command-queue canvas)))
        (back-texture (triple-buffered-value-back (%canvas-triple-texture canvas))))
    (unless (eq front-command-queue (%canvas-last-flushed canvas))
      (setf (%canvas-last-flushed canvas) front-command-queue)
      (bind-framebuffer framebuffer
                        (%canvas-depth-stencil-renderbuffer-id canvas)
                        (unbuffered-texture-id back-texture)
                        (%canvas-width canvas)
                        (%canvas-height canvas))

      (let ((%aw.skia:*canvas* (%canvas-handle canvas))
            (%aw.skia:*paint* nil)
            (%aw.skia:*font* nil))
        (%push-paint)
        (unwind-protect
             (drain-draw-commands front-command-queue)
          (%pop-paint)
          (%aw.skia:reset-transform))
        (%aw.skia:flush-context (canvas-context-handle (%canvas-context canvas))))

      (flush-framebuffer framebuffer)
      (prepare-triple-buffered-value (%canvas-triple-texture canvas)))))


(defun discard-canvas-command-queue (canvas)
  (discard-command-queue (triple-buffered-value-back (%canvas-triple-command-queue canvas))))


(defun push-canvas-command (canvas name &rest args)
  (apply #'push-command
         (triple-buffered-value-back (%canvas-triple-command-queue canvas))
         name args))


(defmacro with-canvas ((canvas) &body body)
  (a:once-only (canvas)
    `(let ((*canvas* ,canvas)
           (%aw.skia:*canvas* (%canvas-handle ,canvas)))
       (prog1 (progn
                (discard-canvas-command-queue ,canvas)
                ,@body)
         (prepare-triple-buffered-value (%canvas-triple-command-queue ,canvas))))))


(defun canvas-texture (canvas)
  (unbuffered-texture-handle (swap-triple-buffered-value (%canvas-triple-texture canvas))))


;;;
;;; CONTEXT
;;;
(atomics:defstruct (canvas-context
                    (:constructor %make-canvas-context))
  framebuffer
  handle
  exit-latch
  done-p
  canvases

  (tasks (make-cas-queue)))


(defun push-canvas-context-task (ctx task)
  (push-cas-queue-value (canvas-context-tasks ctx) task))


(defun push-canvas-context-canvas (ctx canvas)
  (push canvas (canvas-context-canvases ctx)))


(defun delete-canvas-context-canvas (ctx canvas)
  (a:deletef (canvas-context-canvases ctx) canvas))


(defun dump-canvas-context-tasks (ctx)
  (dump-cas-queue-values (canvas-context-tasks ctx)))


(defun drain-tasks (ctx)
  (loop for task in (dump-canvas-context-tasks ctx)
        do (tagbody begin
              (restart-case
                  (funcall task)
                (restart-canvas-task () (go begin))
                (skip-canvas-task () (go end)))
            end)))


(defun draw-canvases (ctx)
  (let ((framebuffer (canvas-context-framebuffer ctx)))
    (loop for canvas in (canvas-context-canvases ctx)
          do (draw-canvas canvas framebuffer)))
  (gl:finish))


(defun maintain-canvas-context (ctx)
  (unwind-protect
       (loop do (drain-tasks ctx)
                (draw-canvases ctx)
                ;; FIXME: need better delta time calc
                (sleep 0.010)
             until (canvas-context-done-p ctx))
    (drain-tasks ctx)))


(defun make-canvas-context (window)
  (let ((ctx (%make-canvas-context :exit-latch (muth:make-latch))))
    (muth:wait-with-latch (ready-latch)
      (%host:make-shared-context-thread
       window
       (lambda ()
         (unwind-protect
              (progn
                (format t "~%Canvas thread init: GL ~A" (gl:get-string :version))
                (finish-output)
                (setf (canvas-context-framebuffer ctx) (make-canvas-context-framebuffer)
                      (canvas-context-handle ctx) (%aw.skia:make-context
                                                   (canvas-context-framebuffer-id
                                                    (canvas-context-framebuffer ctx))))
                (muth:open-latch ready-latch)
                (tagbody begin
                   (restart-case
                       (maintain-canvas-context ctx)
                     (restart-canvas-context-loop () (go begin)))))
           (destroy-canvas-context-framebuffer (canvas-context-framebuffer ctx))
           (muth:open-latch (canvas-context-exit-latch ctx))
           (format t "~%Exiting canvas thread")
           (finish-output)))))
    ctx))


(defun destroy-canvas-context (ctx)
  (setf (canvas-context-done-p ctx) t)
  (muth:wait-for-latch (canvas-context-exit-latch ctx)))


;;;
;;; DRAWING
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expand-relay (function args)
    `(push-canvas-command *canvas* #',function ,@args)))


(defmacro define-command (name-and-opts (&rest lambda-list) &body body)
  (destructuring-bind (name &optional function)
      (a:ensure-list name-and-opts)
    `(defun ,name (,@lambda-list)
       (macrolet ((relay (&rest args)
                    (%expand-relay ',(or function (a:format-symbol :%aw.skia "~A" name)) args)))
         ,@body))))


(defun push-paint ()
  (push-canvas-command *canvas* #'%push-paint))


(defun pop-paint ()
  (push-canvas-command *canvas* #'%pop-paint))


(defmacro with-paint (() &body body)
  `(progn
     (push-paint)
     (unwind-protect
          (progn ,@body)
       (pop-paint))))


(defun paint-color (r g b &optional (a 1f0))
  (push-canvas-command *canvas* #'%aw.skia:paint-color r g b a))


(defun push-font (typeface)
  (push-canvas-command *canvas* #'%push-font typeface))


(defun pop-font ()
  (push-canvas-command *canvas* #'%pop-font))


(defmacro with-font ((typeface) &body body)
  `(progn
     (push-font ,typeface)
     (unwind-protect
          (progn ,@body)
       (pop-font))))


(define-command font-size (size)
  (relay size))


(define-command font-baseline-snap (snapped)
  (relay snapped))


(define-command font-edging (mode)
  (relay mode))


(define-command font-subpixel (subpixeled)
  (relay subpixeled))


(define-command rectangle (x y width height)
  (relay x y width height))


(define-command circle (x y radius)
  (relay x y radius))


(define-command text (x y text)
  (relay text x y))


(define-command (clear-canvas %aw.skia:clear-canvas) ()
  (relay))


(define-command (save-transform %aw.skia:save-transform) ()
  (relay))


(define-command (restore-transform %aw.skia:restore-transform) ()
  (relay))


(defmacro with-saved-transform (() &body body)
  `(progn
     (save-transform)
     (unwind-protect
          (progn ,@body)
       (restore-transform))))


(define-command (translate %aw.skia:translate) (x y)
  (relay x y))


(define-command (rotate %aw.skia:rotate) (angle)
  (relay (* angle *pi-degree*)))


(define-command (rotate-around %aw.skia:rotate-around) (x y angle)
  (relay x y (* angle *pi-degree*)))


(define-command (scale %aw.skia:scale) (x y)
  (relay x y))
