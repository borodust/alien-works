(cl:in-package :alien-works.graphics)


(defclass engine ()
  ((engine :reader handle-of)
   (scene :reader scene-of)
   swap-chain renderer camera view))


;;;
;;; SKYBOX
;;;
(defun make-color-skybox (engine r g b a)
  (%gx:with-vec4f (color r g b a)
    (%gx:with-skybox-builder (%make-skybox (:color color))
      (%make-skybox (handle-of engine)))))


(defun make-cubemap-skybox (engine cubemap)
  (%gx:with-skybox-builder (%make-skybox (:environment cubemap))
    (%make-skybox (handle-of engine))))


(defun (setf skybox) (skybox engine)
  (with-slots (scene) engine
    (setf (%gx:scene-skybox scene) skybox)))


(defun (setf indirect-light) (indirect-light engine)
  (with-slots (scene) engine
    (setf (%gx:scene-indirect-light scene) indirect-light)))


(defmethod initialize-instance :after ((this engine) &key surface)
  (with-slots (engine swap-chain renderer scene camera view) this
    (setf engine (%gx:create-engine)
          swap-chain (%gx:create-swap-chain engine surface)
          renderer (%gx:create-renderer engine)
          camera (%gx:create-camera engine)
          view (%gx:create-view engine)
          scene (%gx:create-scene engine))

    ;; view setup
    (setf (%gx:view-camera view) camera
          (%gx:view-scene view) scene
          (%gx:view-anti-aliasing view) :fxaa
          (%gx:view-post-processing-enabled-p view) t)
    (%gx:update-view-viewport view 0 0 1280 960)

    (%gx:update-camera-lens-projection camera 28f0 (/ 1280 960) 0.01 100)))


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


(defun transform-camera (engine transform)
  (with-slots (camera) engine
    (%gx:with-mat4f (mat transform)
      (%gx:update-camera-model-matrix camera mat))))


(defun add-scene-entity (engine entity)
  (%gx:add-scene-entity (scene-of engine) entity))


(defun transform-entity (engine entity transform)
  (let ((transform-manager (%gx:transform-manager (handle-of engine))))
    (%gx:with-transform-instance (instance entity) transform-manager
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

(define-builder-option width (value))
(define-builder-option height (value))
(define-builder-option depth (value))
(define-builder-option levels (value))
(define-builder-option sampler (value))
(define-builder-option format (value))
(define-builder-option usage (value))
(define-builder-option swizzle (&key r g b a))

(define-builder-option reflections (texture))
(define-builder-option radiance (bands harmonics))
(define-builder-option irradiance (bands harmonics))
(define-builder-option cubemap-irradiance (cubemap))
(define-builder-option rotation (mat3))


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
(defun make-material-from-memory (engine data size)
  (%gx:with-material-builder (%make-material
                              (:package data size))
    (%make-material (handle-of engine))))


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

(defmethod %.material ((this renderable-builder) index material-instance)
  (%gx:renderable-builder-material (handle-of this)
                                   index material-instance))

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


;;;
;;; TEXTURE
;;;
(defclass texture-builder (builder) ())

(defmethod %.width ((this texture-builder) value)
  (%gx:texture-builder-width (handle-of this) value))

(defmethod %.height ((this texture-builder) value)
  (%gx:texture-builder-height (handle-of this) value))

(defmethod %.depth ((this texture-builder) value)
  (%gx:texture-builder-depth (handle-of this) value))

(defmethod %.levels ((this texture-builder) value)
  (%gx:texture-builder-levels (handle-of this) value))

(defmethod %.sampler ((this texture-builder) value)
  (%gx:texture-builder-sampler (handle-of this) (%gx:texture-sampler-type-enum value)))

(defmethod %.format ((this texture-builder) value)
  (%gx:texture-builder-format (handle-of this) (%gx:texture-internal-format-enum value)))

(defmethod %.usage ((this texture-builder) value)
  (%gx:texture-builder-usage (handle-of this) (%gx:texture-usage-enum value)))

(defmethod %.swizzle ((this texture-builder) &key r g b a)
  (%gx:texture-builder-swizzle (handle-of this)
                               (%gx:texture-swizzle-enum (or r :channel-0))
                               (%gx:texture-swizzle-enum (or g :channel-1))
                               (%gx:texture-swizzle-enum (or b :channel-2))
                               (%gx:texture-swizzle-enum (or a :channel-3))))


(defun make-texture (engine &rest options)
  (%gx:with-texture-builder ((%build :instance handle))
    (let ((builder (make-instance 'texture-builder :handle handle)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of engine)))))


(defun destroy-texture (engine texture)
  (%gx:destroy-texture (handle-of engine) texture))


(defun update-texture-image (engine texture level pixel-buffer)
  (%gx:update-texture-image (handle-of engine) texture level pixel-buffer))


(defun update-cubemap-images (engine texture level pixel-buffer face-stride &rest rest-strides)
  (%gx:with-face-offsets (offsets)
    (loop with current-offset = 0
          for sizes = (list* face-stride rest-strides) then (rest sizes)
          for current-size = (or (first sizes) current-size)
          for i from 0 below 6
          do (setf (%gx:face-offset offsets i) current-offset)
             (incf current-offset current-size))
    (%gx:update-cubemap-images (handle-of engine) texture level pixel-buffer offsets)))


(defun generate-texture-mipmaps (engine texture)
  (%gx:generate-texture-mipmaps (handle-of engine) texture))


(defun make-pixel-buffer (data-ptr data-size pixel-format pixel-type &optional release-callback)
  (%gx:make-pixel-buffer data-ptr data-size
                         (%gx:pixel-format-enum pixel-format)
                         (%gx:pixel-type-enum pixel-type)
                         release-callback))


(defun make-compressed-pixel-buffer (data-ptr data-size compressed-size compressed-pixel-type
                                     &optional release-callback)
  (%gx:make-compressed-pixel-buffer data-ptr data-size compressed-size
                                    (%gx:pixel-compressed-type-enum compressed-pixel-type)
                                    release-callback))

(defun destroy-pixel-buffer (pixel-buffer)
  (%gx:destory-pixel-buffer pixel-buffer))

;;;
;;; MATERIAL INSTANCE
;;;
(defun default-material-instance (material)
  (%gx:default-material-instance material))


(defun make-material-instance (material)
  (%gx:make-material-instance material))


(defun destroy-material-instance (engine instance)
  (%gx:destroy-material-instance (handle-of engine) instance))

(defun (setf material-instance-parameter-float) (value material name)
  (setf (%gx:material-instance-parameter-float material name) value))

(defun (setf material-instance-parameter-vec2) (value material name)
  (%gx:with-vec2f (vec (m:vec2 value 0) (m:vec2 value 1))
    (setf (%gx:material-instance-parameter-float2 material name) vec)))

(defun (setf material-instance-parameter-vec3) (value material name)
  (%gx:with-vec3f (vec (m:vec3 value 0) (m:vec3 value 1) (m:vec3 value 2))
   (setf (%gx:material-instance-parameter-float3 material name) vec)))

(defun (setf material-instance-parameter-vec4) (value material name)
  (%gx:with-vec4f (vec (m:vec4 value 0) (m:vec4 value 1) (m:vec4 value 2) (m:vec4 value 3))
    (setf (%gx:material-instance-parameter-float4 material name) vec)))

(defun (setf material-instance-parameter-mat3) (value material name)
  (%gx:with-mat3f (mat value)
    (setf (%gx:material-instance-parameter-mat3 material name) mat)))

(defun (setf material-instance-parameter-mat4) (value material name)
  (%gx:with-mat4f (mat value)
    (setf (%gx:material-instance-parameter-mat4 material name) mat)))

(defun (setf material-instance-parameter-sampler) (value material name texture)
  (setf (%gx:material-instance-parameter-sampler material name texture) value))

;;;
;;; SAMPLER
;;;
(defun make-sampler (&key (min :linear-mipmap-linear)
                       (mag :linear)
                       (s-wrap :repeat)
                       (r-wrap :repeat)
                       (t-wrap :repeat)
                       (compare-mode :none)
                       (compare-func :le))
  (%gx:make-sampler (%gx:min-filter-enum min)
                    (%gx:mag-filter-enum mag)
                    (%gx:wrap-mode-enum s-wrap)
                    (%gx:wrap-mode-enum r-wrap)
                    (%gx:wrap-mode-enum t-wrap)
                    (%gx:compare-mode-enum compare-mode)
                    (%gx:compare-func-enum compare-func)))

(defun destroy-sampler (sampler)
  (%gx:destroy-sampler sampler))

;;;
;;; INDIRECT LIGHT
;;;
(defclass indirect-light-builder (builder) ())

(defmethod %.reflections ((this indirect-light-builder) texture)
  (%gx:indirect-light-reflections (handle-of this) texture))

(defmethod %.radiance ((this indirect-light-builder) bands harmonics)
  (%gx:with-vec3f (vec (m:vec3 harmonics 0) (m:vec3 harmonics 1) (m:vec3 harmonics 2))
    (%gx:indirect-light-radiance (handle-of this) bands vec)))

(defmethod %.irradiance ((this indirect-light-builder) bands harmonics)
  (%gx:with-vec3f (vec (m:vec3 harmonics 0) (m:vec3 harmonics 1) (m:vec3 harmonics 2))
    (%gx:indirect-light-irradiance (handle-of this) bands vec)))

(defmethod %.cubemap-irradiance ((this indirect-light-builder) cubemap)
  (%gx:indirect-light-cubemap-irradiance (handle-of this) cubemap))

(defmethod %.intensity ((this indirect-light-builder) value)
  (%gx:indirect-light-intensity (handle-of this) value))

(defmethod %.rotation ((this indirect-light-builder) value)
  (%gx:with-mat3f (mat value)
    (%gx:indirect-light-rotation (handle-of this) mat)))


(defun make-indirect-light (engine &rest options)
  (%gx:with-indirect-light-builder ((%build :instance handle))
    (let ((builder (make-instance 'indirect-light-builder :handle handle)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of engine)))))


(defun destroy-indirect-light (engine light)
  (%gx:destroy-indirect-light (handle-of engine) light))
