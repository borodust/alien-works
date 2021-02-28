(cl:in-package :alien-works.physics)

;;;
;;; DEMO
;;;
(defun report-actor-position (actor)
  (px:with-transform (transform)
    (px:actor-global-pose transform actor)
    (let ((pos (px:transform-position transform)))
      (format t "~&POSITION: ~8F ~8F ~8F" (px:x pos) (px:y pos) (px:z pos)))
    (finish-output)))


(defun setup-scene (physics dispatcher)
  (px:with-scene-descriptor (descriptor physics dispatcher)
    (px:with-vec3 (vec)
      (setf (px:x vec) 0
            (px:y vec) -9.81
            (px:z vec) 0)
      (setf (px:gravity descriptor) vec))
    (unless (px:scene-descriptor-valid-p descriptor)
      (error "Scene descriptor invalid"))
    (px:make-scene physics descriptor)))


(defun run-simulation (physics)
  (let* ((dispatcher (px:make-cpu-dispatcher 2))
         (scene (setup-scene physics dispatcher))
         (material (px:make-material physics))
         (box (px:add-box physics scene material)))
    (unwind-protect
         (progn
           (report-actor-position box)
           (loop repeat 10
                 do (px:simulate-scene scene 0.14)
                    (px:finish-simulation scene)
                    (report-actor-position box)))
      (px:destroy-material material)
      (px:destroy-scene scene)
      (px:destroy-cpu-dispatcher dispatcher))))


(defun run-with-physics (foundation action)
  (px:with-scale (scale)
    (let* ((pvd (px:make-pvd foundation))
           (connection (px:connect-pvd pvd)))
      (let ((physics (px:make-physics foundation pvd scale)))
        (unwind-protect
             (funcall action physics)
          (px:destroy-physics physics)
          (px:destroy-transport connection)
          (px:destroy-pvd pvd))))))


(defun run-with-foundation (action)
  (flet ((%run-with-foundation (allocator error-callback)
           (let ((foundation (px:make-foundation allocator error-callback)))
             (unwind-protect
                  (funcall action foundation)
               (px:destroy-foundation foundation)))))
    (px:run-with-default-callbacks #'%run-with-foundation)))


(defun run-physics (foundation)
  (run-with-physics foundation #'run-simulation))


(defun run-physics-demo ()
  (run-with-foundation #'run-physics))
