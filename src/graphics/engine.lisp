(cl:in-package :alien-works.graphics)


(defclass engine ()
  (engine swap-chain renderer scene camera view))


(defmethod initialize-instance :after ((this engine) &key surface)
  (with-slots (engine swap-chain renderer scene camera view) this
    (setf engine (fm:create-engine)
          swap-chain (fm:create-swap-chain engine surface)
          renderer (fm:create-renderer engine)
          camera (fm:create-camera engine)
          view (fm:create-view engine)
          scene (fm:create-scene engine))

    ;; scene setup
    (setf (fm:scene-skybox scene) (fm:create-color-skybox engine 0.8 0.25 0.25 1))

    ;; view setup
    (setf (fm:view-camera view) camera
          (fm:view-scene view) scene
          (fm:view-post-processing-enabled-p view) nil)
    (fm:update-view-viewport view 0 0 640 480)))


(defun create-engine (surface)
  (make-instance 'engine :surface surface))


(defun destroy-engine (engine)
  (with-slots (engine swap-chain renderer scene camera view) engine
    (fm:destroy-view engine view)
    (fm:destroy-camera engine camera)
    (fm:destroy-scene engine scene)
    (fm:destroy-renderer engine renderer)
    (fm:destroy-swap-chain engine swap-chain)
    (fm:destroy-engine engine)))


(defun render-frame (engine)
  (with-slots (renderer view swap-chain) engine
    (when (fm:begin-frame renderer swap-chain)
      (fm:render-view renderer view)
      (fm:end-frame renderer))))


(defmacro with-engine ((engine &key (surface (error ":surface missing"))) &body body)
  `(let ((,engine (create-engine ,surface)))
     (unwind-protect
          (progn ,@body)
       (destroy-engine ,engine))))
