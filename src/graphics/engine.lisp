(cl:in-package :alien-works.graphics)


(defclass engine ()
  ((engine :reader handle-of)
   (scene :reader scene-of)
   swap-chain renderer camera view))


(defmethod initialize-instance :after ((this engine) &key surface)
  (with-slots (engine swap-chain renderer scene camera view) this
    (setf engine (%gx:create-engine)
          swap-chain (%gx:create-swap-chain engine surface)
          renderer (%gx:create-renderer engine)
          camera (%gx:create-camera engine)
          view (%gx:create-view engine)
          scene (%gx:create-scene engine))

    ;; scene setup
    (setf (%gx:scene-skybox scene) (%gx:create-color-skybox engine 0.8 0.25 0.25 1))

    ;; view setup
    (setf (%gx:view-camera view) camera
          (%gx:view-scene view) scene
          (%gx:view-post-processing-enabled-p view) nil)
    (%gx:update-view-viewport view 0 0 640 480)))


(defun create-engine (surface)
  (make-instance 'engine :surface surface))


(defun destroy-engine (engine)
  (with-slots (engine swap-chain renderer scene camera view) engine
    (%gx:destroy-view engine view)
    (%gx:destroy-camera engine camera)
    (%gx:destroy-scene engine scene)
    (%gx:destroy-renderer engine renderer)
    (%gx:destroy-swap-chain engine swap-chain)
    (%gx:destroy-engine engine)))


(defun render-frame (engine)
  (with-slots (renderer view swap-chain) engine
    (when (%gx:begin-frame renderer swap-chain)
      (%gx:render-view renderer view)
      (%gx:end-frame renderer))))


(defmacro with-engine ((engine &key (surface (error ":surface missing"))) &body body)
  `(let ((,engine (create-engine ,surface)))
     (unwind-protect
          (progn ,@body)
       (destroy-engine ,engine))))


;;;
;;; TRIANGLE
;;;
(defclass triangle ()
  ((data :initform nil :allocation :class)

   (entity :initform (%gx:create-entity) :reader entity-of)
   (vbuf)
   (ibuf)
   (mat)))


(cffi:defcstruct triangle-vertex
  (pos :float :count 2)
  (color :uint32))


(cffi:defcstruct triangle-data
  (vertices (:struct triangle-vertex) :count 3)
  (indices :uint16 :count 3))


(defun make-triangle-data ()
  (flet ((init-vertex (vertex x y argb)
           (cref:c-val ((vertex (:struct triangle-vertex)))
             (setf (vertex :pos 0) (float x 0f0)
                   (vertex :pos 1) (float y 0f0)
                   (vertex :color) argb))))
    (cref:c-let ((data (:struct triangle-data) :alloc t))
      (init-vertex (data :vertices 0 &)
                   1 0
                   #xffff0000)
      (init-vertex (data :vertices 1 &)
                   (cos (/ (* PI 2) 3)) (sin (/ (* PI 2) 3))
                   #xff00ff00)
      (init-vertex (data :vertices 2 &)
                   (cos (/ (* PI 4) 3)) (sin (/ (* PI 4) 3))
                   #xff0000ff)
      (setf (data :indices 0) 0
            (data :indices 1) 1
            (data :indices 2) 2)
      (data &))))

(defmethod initialize-instance :after ((this triangle) &key engine)
  (with-slots (data entity vbuf ibuf mat) this
    (unless data
      (setf data (make-triangle-data)))
    (let* ((engine (handle-of engine))
           (triangle-vertex-size (cffi:foreign-type-size '(:struct triangle-vertex)))
           (index-size (cffi:foreign-type-size :uint16))
           (pos-size (* 2 (cffi:foreign-type-size :float))))
      (setf vbuf (%gx:with-vertex-buffer-builder
                     (make-vertex-buffer
                      (:vertex-count 3)
                      (:buffer-count 1)
                      (:attribute (%gx:vertex-attribute-enum :position)
                                  0
                                  (%gx:vertex-attribute-type-enum :float2)
                                  0 triangle-vertex-size)
                      (:attribute (%gx:vertex-attribute-enum :color)
                                  0
                                  (%gx:vertex-attribute-type-enum :ubyte4)
                                  pos-size triangle-vertex-size)
                      (:normalized (%gx:vertex-attribute-enum :color) t))
                   (make-vertex-buffer engine))
            ibuf (%gx:with-index-buffer-builder
                     (make-index-buffer
                      (:index-count 3)
                      (:buffer-type (%gx:index-type-enum :ushort)))
                   (make-index-buffer engine))
            mat (%gx:with-material-builder (make-material
                                            (:package (%alien-works.graphics:bundled-material-data
                                                       :defaultmaterial)
                                                      (%alien-works.graphics:bundled-material-size
                                                       :defaultmaterial)))
                  (make-material engine)))

      (cref:c-val ((data (:struct triangle-data)))
        (%gx:update-vertex-buffer vbuf engine 0
                                  (data :vertices &)
                                  (* 3 triangle-vertex-size))
        (%gx:update-index-buffer ibuf engine
                                 (data :indices &)
                                 (* 3 index-size)))


      (%gx:with-box (bounding-box -1 -1 -1 1 1 1)
        (%gx:with-renderable-builder
            (make-renderable
             (:bounding-box bounding-box)
             (:count-bound-geometry
              0 (%gx:renderable-primitive-type-enum :triangles) vbuf ibuf 0 3)
             (:material 0 (%gx:material-default-instance mat))
             (:culling nil)
             (:receive-shadows nil)
             (:cast-shadows nil))
          (make-renderable engine entity))))))


(defun add-triangle (engine)
  (let ((instance (make-instance 'triangle :engine engine)))
    (%gx:add-scene-entity (scene-of engine) (entity-of instance))
    instance))
