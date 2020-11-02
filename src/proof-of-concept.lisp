(cl:defpackage :alien-works
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:aw.host :alien-works.host)
                    (:aw.gx :alien-works.graphics))
  (:use :cl)
  (:export #:run))
(cl:in-package :alien-works)


;;;
;;; UTIL
;;;
(defun load-blobs ()
  (list
   (cffi:load-foreign-library
    "libSDL2.so")
   (cffi:load-foreign-library
    "/home/borodust/devel/repo/claw-filament/src/lib/build/libfilament.clawed.so")
   (cffi:load-foreign-library
    "/home/borodust/devel/repo/claw-physx/src/adapter.so")))

;;;
;;; DEMO
;;;
(defun run ()
  (aw.host:with-window (win)
    (aw.gx:with-engine (engine :surface (aw.host:window-surface win))
      (loop repeat 5
            do (aw.gx:render-frame engine)
               (sleep 1)))))
