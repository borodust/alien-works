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
    (%gx:update-view-viewport view 0 0 640 480)

    (let ((zoom 1.5)
          (aspect (/ 640 480)))
      (%gx:update-camera-projection camera
                                    (%gx:projection-enum :ortho)
                                    (- (* aspect zoom)) (* aspect zoom)
                                    (- zoom) zoom
                                    0 1))))


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

   (engine)
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

(defparameter *baked-material-source*
"
material {
    name : bakedColor,
    requires : [
        color
    ],
    shadingModel : unlit,
    culling : none
}

fragment {
    void material(inout MaterialInputs material) {
        prepareMaterial(material);
        material.baseColor = getColor();
    }
}
")

(defmethod initialize-instance :after ((this triangle) &key engine)
  (with-slots (data entity vbuf ibuf mat (this-engine engine)) this
    (unless data
      (setf data (make-triangle-data)))
    (let* ((engine (handle-of engine))
           (triangle-vertex-size (cffi:foreign-type-size '(:struct triangle-vertex)))
           (index-size (cffi:foreign-type-size :uint16))
           (pos-size (* 2 (cffi:foreign-type-size :float))))
      (setf this-engine engine
            vbuf (%gx:with-vertex-buffer-builder
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
            mat (%gx:with-parsed-material (mat-data *baked-material-source*)
                  (%gx:with-material-builder (make-material
                                              (:package (%gx:material-data mat-data)
                                                        (%gx:material-size mat-data)))
                    (make-material engine))))

      (cref:c-val ((data (:struct triangle-data)))
        (%gx:update-vertex-buffer vbuf engine 0
                                  (data :vertices &)
                                  (* 3 triangle-vertex-size))
        (%gx:update-index-buffer ibuf engine
                                 (data :indices &)
                                 (* 3 index-size)))

      (%gx:with-box (bounding-box -1 -1 -1 1 1 1)
        (%gx:with-renderable-builder
            (%make-renderable (1)
              (:bounding-box bounding-box)
              (:count-bound-geometry
               0 (%gx:renderable-primitive-type-enum :triangles) vbuf ibuf 0 3)
              (:material 0 (%gx:material-default-instance mat))
              (:culling nil)
              (:receive-shadows nil)
              (:cast-shadows nil))
          (%make-renderable engine entity))))))


(defun add-triangle (engine)
  (let ((instance (make-instance 'triangle :engine engine)))
    (%gx:add-scene-entity (scene-of engine) (entity-of instance))
    instance))


(defun add-renderable (engine renderable)
  (%gx:add-scene-entity (scene-of engine) renderable))


(defun rotate-triangle (triangle)
  (with-slots (entity engine) triangle
    (let ((transform-manager (%gx:transform-manager engine)))
      (%gx:with-transform-instance (instance entity) transform-manager
        (m:with-mat4 (source)
          (m:with-vec3 (vec :z 1)
            (m:rotate-mat4 source source (/ (get-internal-real-time) 1000000) vec))
          (%gx:with-mat4f (mat source)
            (setf (%gx:transform transform-manager instance) mat)))))))


;;;
;;; BUILDER
;;;
(defmacro define-builder-option (name lambda-list)
  (let* ((option-name (a:symbolicate '\. name))
         (%option-name (a:symbolicate '% option-name)))
    (multiple-value-bind (required optional rest keys)
        (a:parse-ordinary-lambda-list lambda-list :normalize t)
      `(progn
         (defgeneric ,%option-name (object ,@lambda-list))
         (defun ,option-name (,@lambda-list)
           (lambda (builder)
             (,@(if rest
                    `(apply #',%option-name)
                    `(,%option-name))
              builder
              ,@(when required required)
              ,@(when optional optional)
              ,@(when keys (loop for ((name key)) in keys
                                 append `(,name ,key)))
              ,@(when rest `(,rest)))))))))


(define-builder-option vertex-count (count))
(define-builder-option attribute (attribute type offset stride))
(define-builder-option normalized (attribute normalized-p))

(define-builder-option index-count (count))
(define-builder-option type (type))

(define-builder-option geometry (index type vertices indices))
(define-builder-option index-bound-geometry (index
                                             type
                                             vertices
                                             indices
                                             offset
                                             min-index
                                             max-index
                                             count))
(define-builder-option count-bound-geometry (index type vertices indices offset count))
(define-builder-option material (index material-instance))
(define-builder-option blend-order (index order))
(define-builder-option bounding-box (x-min y-min z-min x-max y-max z-max))
(define-builder-option layer-mask (select values))
(define-builder-option priority (priority))
(define-builder-option culling (enabled))
(define-builder-option cast-shadows (enabled))
(define-builder-option receive-shadows (enabled))
(define-builder-option screen-space-contact-shadows (enabled))
(define-builder-option transform-skinning (bone-count transforms))
(define-builder-option bone-skinning (bone-count bones))
(define-builder-option skinning (bone-count))
(define-builder-option morphing (enabled))



(defclass builder ()
  ((handle :initarg :handle :initform (error ":handle missing") :reader handle-of)))


;;;
;;; VERTEX BUFFER
;;;
(defclass vertex-buffer-builder (builder) ())


(defmethod %.vertex-count ((this vertex-buffer-builder) count)
  (%gx:vertex-buffer-builder-vertex-count (handle-of this) count))


(defmethod %.attribute ((this vertex-buffer-builder) attribute type offset stride)
  (%gx:vertex-buffer-builder-attribute (handle-of this)
                                       (%gx:vertex-attribute-enum attribute)
                                       0
                                       (%gx:vertex-attribute-type-enum type)
                                       offset stride))


(defmethod %.normalized ((this vertex-buffer-builder) attribute normalized-p)
  (%gx:vertex-buffer-builder-normalized (handle-of this)
                                        (%gx:vertex-attribute-enum attribute) normalized-p))


(defun make-vertex-buffer (engine vertex-count &rest options)
  (%gx:with-vertex-buffer-builder ((%make-vertex-buffer :instance handle)
                                   (:buffer-count 1)
                                   (:vertex-count vertex-count))
    (let ((builder (make-instance 'vertex-buffer-builder :handle handle)))
      (loop for step in options
            do (funcall step builder)))
    (%make-vertex-buffer (handle-of engine))))


(defun destroy-vertex-buffer (engine buffer)
  (%gx:destroy-vertex-buffer (handle-of engine) buffer))


(defun fill-vertex-buffer (engine buffer data-ptr data-size &optional (offset 0))
  (%gx:update-vertex-buffer buffer (handle-of engine) 0 data-ptr data-size offset))

;;;
;;; INDEX BUFFER
;;;
(defclass index-buffer-builder (builder) ())


(defmethod %.index-count ((this index-buffer-builder) count)
  (%gx:index-buffer-builder-index-count (handle-of this) count))


(defmethod %.type ((this index-buffer-builder) type)
  (%gx:index-buffer-builder-buffer-type (handle-of this) (%gx:index-type-enum type)))


(defun make-index-buffer (engine index-count &rest options)
  (%gx:with-index-buffer-builder ((%make-index-buffer :instance handle)
                                  (:index-count index-count))
    (let ((builder (make-instance 'index-buffer-builder :handle handle)))
      (loop for step in options
            do (funcall step builder)))
    (%make-index-buffer (handle-of engine))))


(defun destroy-index-buffer (engine buffer)
  (%gx:destroy-index-buffer (handle-of engine) buffer))


(defun fill-index-buffer (engine buffer data-ptr data-size &optional (offset 0))
  (%gx:update-index-buffer buffer (handle-of engine) data-ptr data-size offset))

;;;
;;; MATERIAL
;;;
(defun make-material (engine source &optional base-path)
  (%gx:with-parsed-material (material source base-path)
    (%gx:with-material-builder (%make-material
                                (:package (%gx:material-data material)
                                          (%gx:material-size material)))
      (%make-material (handle-of engine)))))

;;;
;;; RENDERABLE
;;;
(defclass renderable-builder (builder) ())

(defmethod %.geometry ((this renderable-builder) index type vertices indices)
  (%gx:renderable-builder-geometry (handle-of this)
                                   index (%gx:renderable-primitive-type-enum type) vertices indices))

(defmethod %.index-bound-geometry ((this renderable-builder)
                                   index type vertices indices offset min-index max-index count)
  (%gx:renderable-builder-index-bound-geometry (handle-of this)
                                               index
                                               (%gx:renderable-primitive-type-enum type)
                                               vertices indices
                                               offset min-index max-index count))

(defmethod %.count-bound-geometry ((this renderable-builder) index type vertices indices offset count)
  (%gx:renderable-builder-count-bound-geometry (handle-of this)
                                               index
                                               (%gx:renderable-primitive-type-enum type)
                                               vertices offset indices count))

(defmethod %.material ((this renderable-builder) index material)
  (%gx:renderable-builder-material (handle-of this)
                                   index (%gx:material-default-instance material)))

(defmethod %.bounding-box ((this renderable-builder) x-min y-min z-min x-max y-max z-max)
  (%gx:with-box (bounding-box x-min y-min z-min x-max y-max z-max)
    (%gx:renderable-builder-bounding-box (handle-of this) bounding-box)))

(defmethod %.layer-mask ((this renderable-builder) select values)
  (%gx:renderable-builder-layer-mask (handle-of this) select values))

(defmethod %.priority ((this renderable-builder) priority)
  (%gx:renderable-builder-priority (handle-of this) priority))

(defmethod %.culling ((this renderable-builder) enabled)
  (%gx:renderable-builder-culling (handle-of this) enabled))

(defmethod %.cast-shadows ((this renderable-builder) enabled)
  (%gx:renderable-builder-cast-shadows (handle-of this) enabled))

(defmethod %.receive-shadows ((this renderable-builder) enabled)
  (%gx:renderable-builder-receive-shadows (handle-of this) enabled))

(defmethod %.screen-space-contact-shadows ((this renderable-builder) enabled)
  (%gx:renderable-builder-screen-space-contact-shadows (handle-of this) enabled))

(defmethod %.transform-skinning ((this renderable-builder) bone-count transforms)
  ;; FIXME: convert transforms into filament format
  (%gx:renderable-builder-transform-skinning (handle-of this) bone-count transforms))

(defmethod %.bone-skinning ((this renderable-builder) bone-count bones)
  (%gx:renderable-builder-bone-skinning (handle-of this) bone-count bones))

(defmethod %.skinning ((this renderable-builder) bone-count)
  (%gx:renderable-builder-skinning (handle-of this) bone-count))

(defmethod %.morphing ((this renderable-builder) enabled)
  (%gx:renderable-builder-morphing (handle-of this) enabled))

(defmethod %.blend-order ((this renderable-builder) index order)
  (%gx:renderable-builder-blend-order (handle-of this) index order))


(defun make-renderable (engine count &rest options)
  (%gx:with-renderable-builder ((%make-renderable :instance handle) (count))
    (let ((builder (make-instance 'renderable-builder :handle handle))
          (entity (%gx:create-entity)))
      (loop for opt in options
            do (funcall opt builder))
      (%make-renderable (handle-of engine) entity)
      entity)))


(defun destroy-renderable (engine renderable)
  (%gx:destroy-engine-entity engine renderable))
