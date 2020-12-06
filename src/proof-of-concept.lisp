(cl:defpackage :alien-works
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:aw.host :alien-works.host)
                    (:aw.gx :alien-works.graphics))
  (:use :cl)
  (:export #:run))
(cl:in-package :alien-works)

(declaim (special *engine*))


;;;
;;; UTIL
;;;
(defun load-blobs ()
  (list
   (cffi:load-foreign-library
    "libSDL2.so")
   (cffi:load-foreign-library
    "/home/borodust/devel/repo/claw-glm/src/adapter.so")
   (cffi:load-foreign-library
    "/home/borodust/devel/repo/claw-filament/src/lib/build/libfilament.clawed.so")
   (cffi:load-foreign-library
    "/home/borodust/devel/repo/claw-physx/src/adapter.so")))

;;;
;;; DEMO
;;;
(defvar *triangle* nil)


(defun init-loop ()
  (setf *triangle* (aw.gx:add-triangle *engine*)))


(defun handle-event (event)
  (when (and event (eq (aw.host:event-type event) :quit))
    (throw 'quit nil)))


(defun handle-loop ()
  (flet ((%handle-event (event)
           (handle-event event)))
    (aw.host:handle-events #'%handle-event))
  (aw.gx:rotate-triangle *triangle*)
  (aw.gx:render-frame *engine*)
  (sleep 0.014))


(defun run ()
  (aw.host:with-window (win)
    (aw.gx:with-engine (engine :surface (aw.host:window-surface win))
      (let ((*engine* engine))
        (init-loop)
        (catch 'quit
          (loop (handle-loop)))))))
