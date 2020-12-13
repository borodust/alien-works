(cl:in-package :alien-works.graphics)


(defclass engine ()
  ((engine :reader handle-of)
   (scene :reader scene-of)
   swap-chain renderer camera view))


;;;
;;; SKYBOX
;;;
(defun create-color-skybox (engine r g b a)
  (%gx:with-vec4f (color r g b a)
    (%gx:with-skybox-builder (%make-skybox (:color color))
      (%make-skybox engine))))


(defmethod initialize-instance :after ((this engine) &key surface)
  (with-slots (engine swap-chain renderer scene camera view) this
    (setf engine (%gx:create-engine)
          swap-chain (%gx:create-swap-chain engine surface)
          renderer (%gx:create-renderer engine)
          camera (%gx:create-camera engine)
          view (%gx:create-view engine)
          scene (%gx:create-scene engine))

    ;; scene setup
    (setf (%gx:scene-skybox scene) (create-color-skybox engine 0.2 0.2 0.2 1))

    ;; view setup
    (setf (%gx:view-camera view) camera
          (%gx:view-scene view) scene
          (%gx:view-anti-aliasing view) :fxaa
          (%gx:view-post-processing-enabled-p view) t)
    (%gx:update-view-viewport view 0 0 640 480)

    (let ((zoom 1.5)
          (aspect (/ 640 480)))
      (%gx:update-camera-projection camera
                                    (%gx:projection-enum :perspective)
                                    (- (* aspect zoom)) (* aspect zoom)
                                    (- zoom) zoom
                                    1 100))))


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


(defun update-camera-transform (engine transform)
  (with-slots (camera) engine
    (%gx:with-mat4f (mat transform)
      (%gx:update-camera-model-matrix camera mat))))


(defun add-scene-entity (engine entity)
  (%gx:add-scene-entity (scene-of engine) entity))


(defun transform-renderable (engine renderable transform)
  (let ((transform-manager (%gx:transform-manager (handle-of engine))))
    (%gx:with-transform-instance (instance renderable) transform-manager
      (%gx:with-mat4f (mat transform)
        (setf (%gx:transform transform-manager instance) mat)))))


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

(define-builder-option cast-light (enabled))
(define-builder-option position (vec3))
(define-builder-option direction (vec3))
(define-builder-option color (vec3))
(define-builder-option intensity (value))
(define-builder-option intensity-efficiency (intensity efficiency))
(define-builder-option falloff (value))
(define-builder-option spot-light-cone (inner outer))
(define-builder-option sun-angular-radius (radius))
(define-builder-option sun-halo-size (value))
(define-builder-option sun-halo-falloff (value))



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

;;;
;;; LIGHT
;;;
(defclass light-builder (builder) ())

(defmethod %.cast-shadows ((this light-builder) enabled)
  (%gx:light-builder-cast-shadows (handle-of this) enabled))

(defmethod %.cast-light ((this light-builder) enabled)
  (%gx:light-builder-cast-light (handle-of this) enabled))

(defmethod %.position ((this light-builder) vec3)
  (%gx:with-vec3f (vec (m:vec3 vec3 0) (m:vec3 vec3 1) (m:vec3 vec3 2))
    (%gx:light-builder-position (handle-of this) vec)))

(defmethod %.direction ((this light-builder) vec3)
  (%gx:with-vec3f (vec (m:vec3 vec3 0) (m:vec3 vec3 1) (m:vec3 vec3 2))
    (%gx:light-builder-direction (handle-of this) vec)))

(defmethod %.color ((this light-builder) vec3)
  (%gx:with-vec3f (vec (m:vec3 vec3 0) (m:vec3 vec3 1) (m:vec3 vec3 2))
    (%gx:light-builder-color (handle-of this) vec)))

(defmethod %.intensity ((this light-builder) value)
  (%gx:light-builder-intensity (handle-of this) (float value 0f0)))

(defmethod %.intensity-efficiency ((this light-builder) intensity efficiency)
  (%gx:light-builder-intensity-efficiency (handle-of this)
                                          (float intensity 0f0)
                                          (float efficiency 0f0)))

(defmethod %.falloff ((this light-builder) value)
  (%gx:light-builder-falloff (handle-of this) value))

(defmethod %.spot-light-cone ((this light-builder) inner outer)
  (%gx:light-builder-spot-light-cone (handle-of this)
                                     (float inner 0f0)
                                     (float outer 0f0)))

(defmethod %.sun-angular-radius ((this light-builder) value)
  (%gx:light-builder-sun-angular-radius (handle-of this) (float value 0f0)))

(defmethod %.sun-halo-size ((this light-builder) value)
  (%gx:light-builder-sun-halo-size (handle-of this) (float value 0f0)))

(defmethod %.sun-halo-falloff ((this light-builder) value)
  (%gx:light-builder-sun-halo-falloff (handle-of this) (float value 0f0)))

(defun make-light (engine type &rest options)
  (%gx:with-light-builder ((%build :instance handle) (type))
    (let ((builder (make-instance 'light-builder :handle handle))
          (entity (%gx:create-entity)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of engine) entity)
      entity)))

(defun destroy-light (engine light)
  (%gx:destroy-engine-entity engine light))
