(cl:in-package :alien-works.graphics)

(defclass engine ()
  ((engine :reader handle-of)
   (scene :reader scene-of)
   (canvas-context :reader canvas-context-of)
   swap-chain renderer camera camera-entity view))


;;;
;;; SKYBOX
;;;
(defun make-color-skybox (engine r g b a)
  (%fm:with-vec4f (color r g b a)
    (%fm:with-skybox-builder (%make-skybox (:color color))
      (%make-skybox (handle-of engine)))))


(defun make-cubemap-skybox (engine cubemap)
  (%fm:with-skybox-builder (%make-skybox (:environment cubemap))
    (%make-skybox (handle-of engine))))


(defun (setf skybox) (skybox engine)
  (with-slots (scene) engine
    (setf (%fm:scene-skybox scene) skybox)))


(defun (setf indirect-light) (indirect-light engine)
  (with-slots (scene) engine
    (setf (%fm:scene-indirect-light scene) indirect-light)))


(defmethod initialize-instance :after ((this engine) &key window width height)
  (with-slots (engine swap-chain renderer scene camera camera-entity view canvas-context)
      this
    (setf engine (%fm:create-engine (%host:window-graphics-context))
          swap-chain (%fm:create-swap-chain engine (%host:window-surface window))
          renderer (%fm:create-renderer engine)
          camera-entity (%fm:create-entity)
          camera (%fm:create-camera engine camera-entity)
          view (%fm:create-view engine)
          scene (%fm:create-scene engine)
          canvas-context (make-canvas-context window))
    ;; view setup
    (setf (%fm:view-camera view) camera
          (%fm:view-scene view) scene
          (%fm:view-post-processing-enabled-p view) t)
    (%fm:update-view-bloom-options view :enabled t)
    (let ((width (or width 1280))
          (height (or height width 960)))
      (%fm:update-view-viewport view 0 0 width height)
      (%fm:update-camera-lens-projection camera 28f0 (/ width height) 0.01 100))))


(defun create-engine (window &key width height)
  (make-instance 'engine :window window :width width :height height))


(defun destroy-engine (engine)
  (with-slots (engine swap-chain renderer scene camera-entity view canvas-context) engine
    (destroy-canvas-context canvas-context)
    (%fm:destroy-view engine view)
    (%fm:destroy-camera engine camera-entity)
    (%fm:destroy-scene engine scene)
    (%fm:destroy-renderer engine renderer)
    (%fm:destroy-swap-chain engine swap-chain)
    (%fm:destroy-engine engine)))


(defun render-frame (engine)
  (with-slots (renderer view swap-chain) engine
    (when (%fm:begin-frame renderer swap-chain)
      (%fm:render-view renderer view)
      (%fm:end-frame renderer))))


(defmacro with-frame ((engine) &body body)
  `(with-slots (renderer view swap-chain) ,engine
     (when (%fm:begin-frame renderer swap-chain)
       (%fm:render-view renderer view)
       (unwind-protect
            (progn ,@body)
         (%fm:end-frame renderer)))))


(defmacro with-engine ((engine &key (window (error ":window missing")) width height)
                       &body body)
  `(let ((,engine (create-engine ,window :width ,width :height ,height)))
     (unwind-protect
          (progn ,@body)
       (destroy-engine ,engine))))


(defun transform-camera (engine transform)
  (with-slots (camera) engine
    (%fm:with-mat4f (mat transform)
      (%fm:update-camera-model-matrix camera mat))))


(defun camera-lens-projection (engine focal-length aspect near far)
  (with-slots (camera) engine
    (%fm:update-camera-lens-projection camera focal-length aspect near far)))


(defun camera-ortho-projection (engine left right bottom top &key (near 0f0) (far 1f0))
  (with-slots (camera) engine
    (%fm:update-camera-projection camera :ortho left right bottom top near far)))


(defun add-scene-entity (engine entity)
  (%fm:add-scene-entity (scene-of engine) entity))


(defun transform-entity (engine entity transform)
  (let ((transform-manager (%fm:transform-manager (handle-of engine))))
    (%fm:with-transform-instance (instance entity) transform-manager
      (%fm:with-mat4f (mat transform)
        (setf (%fm:transform transform-manager instance) mat)))))


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
(define-builder-option import (value))

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
  (%fm:vertex-buffer-builder-vertex-count (handle-of this) count))


(defmethod %.attribute ((this vertex-buffer-builder) attribute type offset stride)
  (%fm:vertex-buffer-builder-attribute (handle-of this)
                                       (%fm:vertex-attribute-enum attribute)
                                       0
                                       (%fm:vertex-attribute-type-enum type)
                                       offset stride))


(defmethod %.normalized ((this vertex-buffer-builder) attribute normalized-p)
  (%fm:vertex-buffer-builder-normalized (handle-of this)
                                        (%fm:vertex-attribute-enum attribute) normalized-p))


(defun make-vertex-buffer (engine vertex-count &rest options)
  (%fm:with-vertex-buffer-builder ((%make-vertex-buffer :instance handle)
                                   (:buffer-count 1)
                                   (:vertex-count vertex-count))
    (let ((builder (make-instance 'vertex-buffer-builder :handle handle)))
      (loop for step in options
            do (funcall step builder)))
    (%make-vertex-buffer (handle-of engine))))


(defun destroy-vertex-buffer (engine buffer)
  (%fm:destroy-vertex-buffer (handle-of engine) buffer))


(defun fill-vertex-buffer (engine buffer data-ptr data-size &optional (offset 0))
  (%fm:update-vertex-buffer buffer (handle-of engine) 0 data-ptr data-size offset))

;;;
;;; INDEX BUFFER
;;;
(defclass index-buffer-builder (builder) ())


(defmethod %.index-count ((this index-buffer-builder) count)
  (%fm:index-buffer-builder-index-count (handle-of this) count))


(defmethod %.type ((this index-buffer-builder) type)
  (%fm:index-buffer-builder-buffer-type (handle-of this) (%fm:index-type-enum type)))


(defun make-index-buffer (engine index-count &rest options)
  (%fm:with-index-buffer-builder ((%make-index-buffer :instance handle)
                                  (:index-count index-count))
    (let ((builder (make-instance 'index-buffer-builder :handle handle)))
      (loop for step in options
            do (funcall step builder)))
    (%make-index-buffer (handle-of engine))))


(defun destroy-index-buffer (engine buffer)
  (%fm:destroy-index-buffer (handle-of engine) buffer))


(defun fill-index-buffer (engine buffer data-ptr data-size &optional (offset 0))
  (%fm:update-index-buffer buffer (handle-of engine) data-ptr data-size offset))

;;;
;;; MATERIAL
;;;
(defun make-material-from-memory (engine data size)
  (%fm:with-material-builder (%make-material
                              (:package data size))
    (%make-material (handle-of engine))))


;;;
;;; RENDERABLE
;;;
(defclass renderable-builder (builder) ())

(defmethod %.geometry ((this renderable-builder) index type vertices indices)
  (%fm:renderable-builder-geometry (handle-of this)
                                   index (%fm:renderable-primitive-type-enum type) vertices indices))

(defmethod %.index-bound-geometry ((this renderable-builder)
                                   index type vertices indices offset min-index max-index count)
  (%fm:renderable-builder-index-bound-geometry (handle-of this)
                                               index
                                               (%fm:renderable-primitive-type-enum type)
                                               vertices indices
                                               offset min-index max-index count))

(defmethod %.count-bound-geometry ((this renderable-builder) index type vertices indices offset count)
  (%fm:renderable-builder-count-bound-geometry (handle-of this)
                                               index
                                               (%fm:renderable-primitive-type-enum type)
                                               vertices indices offset count))

(defmethod %.material ((this renderable-builder) index material-instance)
  (%fm:renderable-builder-material (handle-of this)
                                   index material-instance))

(defmethod %.bounding-box ((this renderable-builder) x-min y-min z-min x-max y-max z-max)
  (%fm:with-box (bounding-box x-min y-min z-min x-max y-max z-max)
    (%fm:renderable-builder-bounding-box (handle-of this) bounding-box)))

(defmethod %.layer-mask ((this renderable-builder) select values)
  (%fm:renderable-builder-layer-mask (handle-of this) select values))

(defmethod %.priority ((this renderable-builder) priority)
  (%fm:renderable-builder-priority (handle-of this) priority))

(defmethod %.culling ((this renderable-builder) enabled)
  (%fm:renderable-builder-culling (handle-of this) enabled))

(defmethod %.cast-shadows ((this renderable-builder) enabled)
  (%fm:renderable-builder-cast-shadows (handle-of this) enabled))

(defmethod %.receive-shadows ((this renderable-builder) enabled)
  (%fm:renderable-builder-receive-shadows (handle-of this) enabled))

(defmethod %.screen-space-contact-shadows ((this renderable-builder) enabled)
  (%fm:renderable-builder-screen-space-contact-shadows (handle-of this) enabled))

(defmethod %.transform-skinning ((this renderable-builder) bone-count transforms)
  ;; FIXME: convert transforms into filament format
  (%fm:renderable-builder-transform-skinning (handle-of this) bone-count transforms))

(defmethod %.bone-skinning ((this renderable-builder) bone-count bones)
  (%fm:renderable-builder-bone-skinning (handle-of this) bone-count bones))

(defmethod %.skinning ((this renderable-builder) bone-count)
  (%fm:renderable-builder-skinning (handle-of this) bone-count))

(defmethod %.morphing ((this renderable-builder) enabled)
  (%fm:renderable-builder-morphing (handle-of this) enabled))

(defmethod %.blend-order ((this renderable-builder) index order)
  (%fm:renderable-builder-blend-order (handle-of this) index order))


(defun make-renderable (engine count &rest options)
  (%fm:with-renderable-builder ((%make-renderable :instance handle) (count))
    (let ((builder (make-instance 'renderable-builder :handle handle))
          (entity (%fm:create-entity)))
      (loop for opt in options
            do (funcall opt builder))
      (%make-renderable (handle-of engine) entity)
      entity)))


(defun destroy-renderable (engine renderable)
  (%fm:destroy-engine-entity (handle-of engine) renderable))

;;;
;;; LIGHT
;;;
(defclass light-builder (builder) ())

(defmethod %.cast-shadows ((this light-builder) enabled)
  (%fm:light-builder-cast-shadows (handle-of this) enabled))

(defmethod %.cast-light ((this light-builder) enabled)
  (%fm:light-builder-cast-light (handle-of this) enabled))

(defmethod %.position ((this light-builder) vec3)
  (%fm:with-vec3f (vec (m:vec3 vec3 0) (m:vec3 vec3 1) (m:vec3 vec3 2))
    (%fm:light-builder-position (handle-of this) vec)))

(defmethod %.direction ((this light-builder) vec3)
  (%fm:with-vec3f (vec (m:vec3 vec3 0) (m:vec3 vec3 1) (m:vec3 vec3 2))
    (%fm:light-builder-direction (handle-of this) vec)))

(defmethod %.color ((this light-builder) vec3)
  (%fm:with-vec3f (vec (m:vec3 vec3 0) (m:vec3 vec3 1) (m:vec3 vec3 2))
    (%fm:light-builder-color (handle-of this) vec)))

(defmethod %.intensity ((this light-builder) value)
  (%fm:light-builder-intensity (handle-of this) (float value 0f0)))

(defmethod %.intensity-efficiency ((this light-builder) intensity efficiency)
  (%fm:light-builder-intensity-efficiency (handle-of this)
                                          (float intensity 0f0)
                                          (float efficiency 0f0)))

(defmethod %.falloff ((this light-builder) value)
  (%fm:light-builder-falloff (handle-of this) value))

(defmethod %.spot-light-cone ((this light-builder) inner outer)
  (%fm:light-builder-spot-light-cone (handle-of this)
                                     (float inner 0f0)
                                     (float outer 0f0)))

(defmethod %.sun-angular-radius ((this light-builder) value)
  (%fm:light-builder-sun-angular-radius (handle-of this) (float value 0f0)))

(defmethod %.sun-halo-size ((this light-builder) value)
  (%fm:light-builder-sun-halo-size (handle-of this) (float value 0f0)))

(defmethod %.sun-halo-falloff ((this light-builder) value)
  (%fm:light-builder-sun-halo-falloff (handle-of this) (float value 0f0)))

(defun make-light (engine type &rest options)
  (%fm:with-light-builder ((%build :instance handle) (type))
    (let ((builder (make-instance 'light-builder :handle handle))
          (entity (%fm:create-entity)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of engine) entity)
      entity)))

(defun destroy-light (engine light)
  (%fm:destroy-engine-entity (handle-of engine) light))


;;;
;;; TEXTURE
;;;
(defclass texture-builder (builder) ())

(defmethod %.width ((this texture-builder) value)
  (%fm:texture-builder-width (handle-of this) value))

(defmethod %.height ((this texture-builder) value)
  (%fm:texture-builder-height (handle-of this) value))

(defmethod %.depth ((this texture-builder) value)
  (%fm:texture-builder-depth (handle-of this) value))

(defmethod %.levels ((this texture-builder) value)
  (%fm:texture-builder-levels (handle-of this) value))

(defmethod %.sampler ((this texture-builder) value)
  (%fm:texture-builder-sampler (handle-of this) (%fm:texture-sampler-type-enum value)))

(defmethod %.format ((this texture-builder) value)
  (%fm:texture-builder-format (handle-of this) (%fm:texture-internal-format-enum value)))

(defmethod %.usage ((this texture-builder) value)
  (%fm:texture-builder-usage (handle-of this) (%fm:texture-usage-enum value)))

(defmethod %.swizzle ((this texture-builder) &key r g b a)
  (%fm:texture-builder-swizzle (handle-of this)
                               (%fm:texture-swizzle-enum (or r :channel-0))
                               (%fm:texture-swizzle-enum (or g :channel-1))
                               (%fm:texture-swizzle-enum (or b :channel-2))
                               (%fm:texture-swizzle-enum (or a :channel-3))))


(defmethod %.import ((this texture-builder) value)
  (%fm:texture-builder-import (handle-of this) value))


(defun make-texture (engine &rest options)
  (%fm:with-texture-builder ((%build :instance handle))
    (let ((builder (make-instance 'texture-builder :handle handle)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of engine)))))


(defun destroy-texture (engine texture)
  (%fm:destroy-texture (handle-of engine) texture))


(defun update-texture-image (engine texture level pixel-buffer)
  (%fm:update-texture-image (handle-of engine) texture level pixel-buffer))


(defun update-cubemap-images (engine texture level pixel-buffer face-stride &rest rest-strides)
  (%fm:with-face-offsets (offsets)
    (loop with current-offset = 0
          for sizes = (list* face-stride rest-strides) then (rest sizes)
          for current-size = (or (first sizes) current-size)
          for i from 0 below 6
          do (setf (%fm:face-offset offsets i) current-offset)
             (incf current-offset current-size))
    (%fm:update-cubemap-images (handle-of engine) texture level pixel-buffer offsets)))


(defun generate-texture-mipmaps (engine texture)
  (%fm:generate-texture-mipmaps (handle-of engine) texture))


(defun make-pixel-buffer (data-ptr data-size pixel-format pixel-type &optional release-callback)
  (%fm:make-pixel-buffer data-ptr data-size
                         (%fm:pixel-format-enum pixel-format)
                         (%fm:pixel-type-enum pixel-type)
                         release-callback))


(defun make-compressed-pixel-buffer (data-ptr data-size compressed-size compressed-pixel-type
                                     &optional release-callback)
  (%fm:make-compressed-pixel-buffer data-ptr data-size compressed-size
                                    (%fm:pixel-compressed-type-enum compressed-pixel-type)
                                    release-callback))

(defun destroy-pixel-buffer (pixel-buffer)
  (%fm:destory-pixel-buffer pixel-buffer))

;;;
;;; MATERIAL INSTANCE
;;;
(defun default-material-instance (material)
  (%fm:default-material-instance material))


(defun make-material-instance (material)
  (%fm:make-material-instance material))


(defun destroy-material-instance (engine instance)
  (%fm:destroy-material-instance (handle-of engine) instance))

(defun (setf material-instance-parameter-float) (value material name)
  (setf (%fm:material-instance-parameter-float material name) value))

(defun (setf material-instance-parameter-vec2) (value material name)
  (%fm:with-vec2f (vec (m:vec2 value 0) (m:vec2 value 1))
    (setf (%fm:material-instance-parameter-float2 material name) vec)))

(defun (setf material-instance-parameter-vec3) (value material name)
  (%fm:with-vec3f (vec (m:vec3 value 0) (m:vec3 value 1) (m:vec3 value 2))
   (setf (%fm:material-instance-parameter-float3 material name) vec)))

(defun (setf material-instance-parameter-vec4) (value material name)
  (%fm:with-vec4f (vec (m:vec4 value 0) (m:vec4 value 1) (m:vec4 value 2) (m:vec4 value 3))
    (setf (%fm:material-instance-parameter-float4 material name) vec)))

(defun (setf material-instance-parameter-mat3) (value material name)
  (%fm:with-mat3f (mat value)
    (setf (%fm:material-instance-parameter-mat3 material name) mat)))

(defun (setf material-instance-parameter-mat4) (value material name)
  (%fm:with-mat4f (mat value)
    (setf (%fm:material-instance-parameter-mat4 material name) mat)))

(defun (setf material-instance-parameter-sampler) (value material name texture)
  (setf (%fm:material-instance-parameter-sampler material name texture) value))

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
  (%fm:make-sampler (%fm:min-filter-enum min)
                    (%fm:mag-filter-enum mag)
                    (%fm:wrap-mode-enum s-wrap)
                    (%fm:wrap-mode-enum r-wrap)
                    (%fm:wrap-mode-enum t-wrap)
                    (%fm:compare-mode-enum compare-mode)
                    (%fm:compare-func-enum compare-func)))

(defun destroy-sampler (sampler)
  (%fm:destroy-sampler sampler))

;;;
;;; INDIRECT LIGHT
;;;
(defclass indirect-light-builder (builder) ())

(defmethod %.reflections ((this indirect-light-builder) texture)
  (%fm:indirect-light-reflections (handle-of this) texture))

(defmethod %.radiance ((this indirect-light-builder) bands harmonics)
  (%fm:with-vec3f (vec (m:vec3 harmonics 0) (m:vec3 harmonics 1) (m:vec3 harmonics 2))
    (%fm:indirect-light-radiance (handle-of this) bands vec)))

(defmethod %.irradiance ((this indirect-light-builder) bands harmonics)
  (%fm:with-vec3f (vec (m:vec3 harmonics 0) (m:vec3 harmonics 1) (m:vec3 harmonics 2))
    (%fm:indirect-light-irradiance (handle-of this) bands vec)))

(defmethod %.cubemap-irradiance ((this indirect-light-builder) cubemap)
  (%fm:indirect-light-cubemap-irradiance (handle-of this) cubemap))

(defmethod %.intensity ((this indirect-light-builder) value)
  (%fm:indirect-light-intensity (handle-of this) value))

(defmethod %.rotation ((this indirect-light-builder) value)
  (%fm:with-mat3f (mat value)
    (%fm:indirect-light-rotation (handle-of this) mat)))


(defun make-indirect-light (engine &rest options)
  (%fm:with-indirect-light-builder ((%build :instance handle))
    (let ((builder (make-instance 'indirect-light-builder :handle handle)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of engine)))))


(defun destroy-indirect-light (engine light)
  (%fm:destroy-indirect-light (handle-of engine) light))
