(cl:in-package :alien-works.graphics)

(declaim (special *engine*))

(defvar *engine-tasks* (mt:make-guarded-reference nil))

(defvar *renderer* nil)


(defmacro with-transform ((transform &rest operations) &body body)
  (alexandria:with-gensyms (transform0 transform1 vec)
    (flet ((%expand-transform (result source operation-desc)
             (let* ((operation (first operation-desc)))
               (if (eq operation :transform)
                   `(m:mat4-mult ,result ,source ,(second operation-desc))
                   (let ((vec-config (if (eq operation :rotate)
                                         (cddr operation-desc)
                                         (rest operation-desc))))
                     (destructuring-bind (&key x y z) vec-config
                       `(m:with-vec3 (,vec
                                      ,@(when x `(:x ,x))
                                      ,@(when y `(:y ,y))
                                      ,@(when z `(:z ,z)))
                          ,(ecase operation
                             (:rotate `(m:rotate-mat4 ,result ,source ,(second operation-desc) ,vec))
                             (:translate `(m:translate-mat4 ,result ,source ,vec))
                             (:scale `(m:scale-mat4 ,result ,source ,vec))))))))))
      `(m:with-mat4* (,transform0
                      ,transform1)
         ,@(loop with result = transform0 and source = transform1
                 for operation in operations
                 collect (prog1 (%expand-transform result source operation)
                           (rotatef result source))
                   into transforms
                 finally (return (append transforms
                                         `((let ((,transform ,source))
                                             ,@body)))))))))


(defclass engine ()
  ((engine :reader handle-of)
   (canvas-context :reader canvas-context-of)
   (renderer :reader renderer-of)
   swap-chain))


(defun engine-handle ()
  (handle-of *engine*))


(defun renderer-handle ()
  (renderer-of *engine*))


(defmethod initialize-instance :after ((this engine) &key)
  (with-slots (engine swap-chain renderer canvas-context) this
    (setf engine (%fm:create-engine (%host:window-graphics-context))
          swap-chain (%fm:create-swap-chain engine (%host:window-surface))
          renderer (%fm:create-renderer engine)
          canvas-context (make-canvas-context)))
  (update-material-parameter-registry)
  (mt:with-guarded-reference (tasks *engine-tasks*)
    (setf tasks (list nil))))


(defun create-engine (&key)
  (make-instance 'engine))


(defun destroy-engine (engine)
  (with-slots (engine swap-chain renderer canvas-context) engine
    (destroy-canvas-context canvas-context)
    (%fm:destroy-renderer engine renderer)
    (%fm:destroy-swap-chain engine swap-chain)
    (%fm:destroy-engine engine)
    (mt:with-guarded-reference (tasks *engine-tasks*)
      (setf tasks nil))))


(defun consume-engine-tasks ()
  (mapc #'funcall (mt:with-guarded-reference (tasks *engine-tasks*)
                    (prog1 (rest tasks)
                      (setf (rest tasks) nil)))))


(defun push-engine-task (task)
  (mt:with-guarded-reference (tasks *engine-tasks*)
    (when tasks
      (a:nconcf (rest tasks) (list task)))))


(defmacro with-frame-loop (&body body)
  `(progn
     ;; end outer frame earlier
     (when *renderer*
       (%fm:end-frame *renderer*))
     (unwind-protect
          (progn ,@body)
       (when *renderer*
         ;; restart outer frame
         (unless (%fm:begin-frame *renderer* (slot-value *engine* 'swap-chain))
           (throw 'skip-frame (values)))))))


(defmacro when-frame (() &body body)
  `(with-slots (renderer swap-chain) *engine*
     (when (%fm:begin-frame renderer swap-chain)
       (unwind-protect
            (catch 'skip-frame
              (let ((*renderer* renderer))
                (consume-engine-tasks)
                ,@body))
         (%fm:end-frame renderer)))))


(defmacro %graphics:with-renderer (() &body body)
  `(let ((*engine* (create-engine)))
     (unwind-protect
          (progn ,@body)
       (destroy-engine *engine*))))


(defun transform-entity (entity transform)
  (let ((transform-manager (%fm:transform-manager (handle-of *engine*))))
    (%fm:with-transform-instance (instance entity) transform-manager
      (%fm:with-mat4f (mat transform)
        (setf (%fm:transform transform-manager instance) mat)))))


(defun create-entity ()
  (let* ((transform-manager (%fm:transform-manager (handle-of *engine*)))
         (entity-manager (%fm:entity-manager (handle-of *engine*)))
         (entity (%fm:create-entity entity-manager)))
    (%fm:attach-transform transform-manager entity)
    entity))


(defun destroy-entity (entity)
  (%fm:destroy-engine-entity (handle-of *engine*) entity)
  (let ((entity-manager (%fm:entity-manager (handle-of *engine*))))
    (%fm:destroy-entity entity-manager entity)))


(defun (setf entity-parent) (parent entity)
  (let ((transform-manager (%fm:transform-manager (handle-of *engine*))))
    (%fm:with-transform-instance (entity-instance entity) transform-manager
      (if parent
          (%fm:with-transform-instance (parent-instance parent) transform-manager
            (setf (%fm:transform-parent transform-manager entity-instance)
                  parent-instance))
          (iffi:with-intricate-instance (parent-instance
                                         %filament:transform-manager+instance)
            (setf (%fm:transform-parent transform-manager entity-instance)
                  parent-instance))))))


(defun entity-parent (entity)
  (declare (ignore entity))
  (error "Not implemented"))


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
(define-builder-option attribute-from-layout (attribute type layout slot))
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


(defmethod %.attribute-from-layout
    ((this vertex-buffer-builder) attribute type layout-name slot-name)
  (%fm:vertex-buffer-builder-attribute
   (handle-of this)
   (%fm:vertex-attribute-enum attribute)
   0
   (%fm:vertex-attribute-type-enum type)
   (mem:memory-layout-slot-offset layout-name slot-name)
   (mem:memory-layout-size layout-name)))


(defmethod %.normalized ((this vertex-buffer-builder) attribute normalized-p)
  (%fm:vertex-buffer-builder-normalized (handle-of this)
                                        (%fm:vertex-attribute-enum attribute) normalized-p))


(defun make-vertex-buffer (vertex-count &rest options)
  (%fm:with-vertex-buffer-builder ((%make-vertex-buffer :instance handle)
                                   (:buffer-count 1)
                                   (:vertex-count vertex-count))
    (let ((builder (make-instance 'vertex-buffer-builder :handle handle)))
      (loop for step in options
            do (funcall step builder)))
    (%make-vertex-buffer (handle-of *engine*))))


(defun destroy-vertex-buffer (buffer)
  (%fm:destroy-vertex-buffer (handle-of *engine*) buffer))


(defun fill-vertex-buffer (buffer memory-vector &key (vector-start 0) vector-end
                                                  (buffer-offset 0)
                                                  when-done)
  (%fm:update-vertex-buffer buffer (handle-of *engine*) 0
                            (%mem:memory-vector-pointer memory-vector vector-start)
                            (- (or vector-end (length memory-vector)) vector-start)
                            buffer-offset
                            when-done))

;;;
;;; INDEX BUFFER
;;;
(defclass index-buffer-builder (builder) ())


(defmethod %.index-count ((this index-buffer-builder) count)
  (%fm:index-buffer-builder-index-count (handle-of this) count))


(defmethod %.type ((this index-buffer-builder) type)
  (%fm:index-buffer-builder-buffer-type (handle-of this) (%fm:index-type-enum type)))


(defun make-index-buffer (index-count &rest options)
  (%fm:with-index-buffer-builder ((%make-index-buffer :instance handle)
                                  (:index-count index-count))
    (let ((builder (make-instance 'index-buffer-builder :handle handle)))
      (loop for step in options
            do (funcall step builder)))
    (%make-index-buffer (handle-of *engine*))))


(defun destroy-index-buffer (buffer)
  (%fm:destroy-index-buffer (handle-of *engine*) buffer))


(defun fill-index-buffer (buffer memory-vector &key (vector-start 0) vector-end
                                                 (buffer-offset 0)
                                                 when-done)
  (%fm:update-index-buffer buffer (handle-of *engine*)
                           (%mem:memory-vector-pointer memory-vector vector-start)
                           (- (or vector-end (length memory-vector)) vector-start)
                           buffer-offset
                           when-done))

;;;
;;; MATERIAL
;;;
(defun make-material-from-memory (data size)
  (%fm:with-material-builder (%make-material
                              (:package data size))
    (%make-material (handle-of *engine*))))


(defun make-material-from-memory-vector (data)
  (assert (or (subtypep (array-element-type data) '(unsigned-byte 8))
              (subtypep (array-element-type data) '(signed-byte 8))))
  (u:with-pinned-array-pointer (ptr data)
    (make-material-from-memory ptr (length data))))


(defun material-name (material)
  (with-input-from-string (in (%fm:material-name material))
    (read in)))


(defun destroy-material (material)
  (%fm:destroy-material (handle-of *engine*) material))


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

(defmethod %.blend-order ((this renderable-builder) index order)
  (%fm:renderable-builder-blend-order (handle-of this) index order))


(defun make-renderable (count &rest options)
  (%fm:with-renderable-builder ((%make-renderable :instance handle) (count))
    (let* ((builder (make-instance 'renderable-builder :handle handle))
           (entity-manager (%fm:entity-manager (handle-of *engine*)))
           (entity (%fm:create-entity entity-manager)))
      (loop for opt in options
            do (funcall opt builder))
      (%make-renderable (handle-of *engine*) entity)
      (let ((transform-manager (%fm:transform-manager (handle-of *engine*))))
        (%fm:attach-transform transform-manager entity))
      entity)))


(defun renderable-material-instance (renderable layer)
  (let ((manager (%fm:renderable-manager (handle-of *engine*))))
    (%fm:with-renderable-instance (instance renderable) manager
      (%fm:renderable-material-instance manager instance layer))))


(defun (setf renderable-material-instance) (material-instance renderable layer)
  (let ((manager (%fm:renderable-manager (handle-of *engine*))))
    (%fm:with-renderable-instance (instance renderable) manager
      (setf
       (%fm:renderable-material-instance manager instance layer)
       material-instance))))


(defun destroy-renderable (renderable)
  (%fm:destroy-engine-entity (handle-of *engine*) renderable))

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
  (%fm:light-builder-falloff (handle-of this) (float value 0f0)))

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

(defun make-light (type &rest options)
  (%fm:with-light-builder ((%build :instance handle) (type))
    (let* ((builder (make-instance 'light-builder :handle handle))
           (entity-manager (%fm:entity-manager (handle-of *engine*)))
           (entity (%fm:create-entity entity-manager)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of *engine*) entity)
      (let ((transform-manager (%fm:transform-manager (handle-of *engine*))))
        (%fm:attach-transform transform-manager entity))
      entity)))

(defun destroy-light (light)
  (%fm:destroy-engine-entity (handle-of *engine*) light))


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


(defun make-texture (&rest options)
  (%fm:with-texture-builder ((%build :instance handle))
    (let ((builder (make-instance 'texture-builder :handle handle)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of *engine*)))))


(defun decode-texture (octet-vector &key (internal-format :etc2-eac-rgba8)
                                      (transfer-function :linear))
  (%fm:with-compressed-texture-decoder (decoder (handle-of *engine*) internal-format)
    (let ((result (%fm:decode-compressed-texture decoder octet-vector transfer-function)))
      (unless (cffi:null-pointer-p result)
        result))))


(defun destroy-texture (texture)
  (%fm:destroy-texture (handle-of *engine*) texture))


(defun update-texture-image (texture level pixel-buffer)
  (%fm:update-texture-image (handle-of *engine*) texture level pixel-buffer))


(defun update-cubemap-images (texture level pixel-buffer face-stride &rest rest-strides)
  (%fm:with-face-offsets (offsets)
    (loop with current-offset = 0
          for sizes = (list* face-stride rest-strides) then (rest sizes)
          for current-size = (or (first sizes) current-size)
          for i from 0 below 6
          do (setf (%fm:face-offset offsets i) current-offset)
             (incf current-offset current-size))
    (%fm:update-cubemap-images (handle-of *engine*) texture level pixel-buffer offsets)))


(defun generate-texture-mipmaps (texture)
  (%fm:generate-texture-mipmaps (handle-of *engine*) texture))


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


(defun destroy-material-instance (instance)
  (%fm:destroy-material-instance (handle-of *engine*) instance))


(defun material-instance-name (instance)
  (with-input-from-string (in (%fm:material-instance-name instance))
    (read in)))


(defun material-instance-parameter (material name)
  (declare (ignore material name))
  (error "Not implemented: write only"))


(defun (setf material-instance-parameter) (value material parameter-name
                                           &optional texture)
  (let ((material-name (material-instance-name material)))
    (multiple-value-bind (type glsl-name)
        (find-material-parameter-type material-name parameter-name)
      (unless type
        (error "Parameter with name ~A not found in material ~A"
               material-name parameter-name))
      (ecase type
        (:float (setf
                 (%fm:material-instance-parameter-float material glsl-name)
                 value))
        (:vec2 (setf
                (%fm:material-instance-parameter-float2 material glsl-name)
                value))
        (:vec3 (setf
                (%fm:material-instance-parameter-float3 material glsl-name)
                value))
        (:vec4 (setf
                (%fm:material-instance-parameter-float4 material glsl-name)
                value))
        (:mat3 (setf
                (%fm:material-instance-parameter-mat3 material glsl-name)
                value))
        (:mat4 (setf
                (%fm:material-instance-parameter-mat4 material glsl-name)
                value))
        ((:sampler-2d
          :sampler-2d-array
          :sampler-external
          :sampler-cubemap)
         (setf
          (%fm:material-instance-parameter-sampler material glsl-name texture)
          value)))))
  value)


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


(defun make-indirect-light (&rest options)
  (%fm:with-indirect-light-builder ((%build :instance handle))
    (let ((builder (make-instance 'indirect-light-builder :handle handle)))
      (loop for opt in options
            do (funcall opt builder))
      (%build (handle-of *engine*)))))


(defun destroy-indirect-light (light)
  (%fm:destroy-indirect-light (handle-of *engine*) light))


;;;
;;; SCENE
;;;
(defclass scene ()
  ((engine :initarg :engine)
   (scene :reader handle-of)
   view
   camera
   camera-entity))


(defmethod initialize-instance :after ((this scene) &key width height
                                                      (post-processing-enabled t)
                                                      (shadows-enabled t)
                                                      (blend-mode :opaque))
  (with-slots (engine scene camera view camera-entity) this
    (let ((entity-manager (%fm:entity-manager (handle-of *engine*))))
      (setf camera-entity (%fm:create-entity entity-manager)
            camera (%fm:create-camera engine camera-entity)
            view (%fm:create-view engine)
            scene (%fm:create-scene engine)

            (%fm:view-camera view) camera
            (%fm:view-scene view) scene
            (%fm:view-post-processing-enabled-p view) post-processing-enabled
            (%fm:view-shadows-enabled-p view) shadows-enabled
            (%fm:view-blend-mode view) blend-mode))
    (when post-processing-enabled
      (%fm:update-view-bloom-options view :enabled t))
    (let ((width (or width 1280))
          (height (or height width 960)))
      (%fm:update-view-viewport view 0 0 width height)
      (%fm:update-camera-lens-projection camera 28f0 (/ width height) 0.01 100))))


(defun make-scene (viewport-width viewport-height
                   &key (post-processing t)
                     (shadows t)
                     (blend-mode :opaque))
  (make-instance 'scene :engine (handle-of *engine*)
                        :width viewport-width
                        :height viewport-height
                        :post-processing-enabled post-processing
                        :shadows-enabled shadows
                        :blend-mode blend-mode))


(defun destroy-scene (scene)
  (with-slots (engine view scene camera-entity camera) scene
    (let ((entity-manager (%fm:entity-manager (handle-of *engine*))))
      (%fm:destroy-camera engine camera-entity)
      (%fm:destroy-entity entity-manager camera-entity)
      (%fm:destroy-scene engine scene)
      (%fm:destroy-view engine view))))


(defun (setf scene-skybox) (skybox scene)
  (with-slots (scene) scene
    (setf (%fm:scene-skybox scene) skybox)))


(defun scene-skybox (scene)
  (with-slots (scene) scene
    (%fm:scene-skybox scene)))


(defun (setf scene-indirect-light) (indirect-light scene)
  (with-slots (scene) scene
    (setf (%fm:scene-indirect-light scene) indirect-light)))


(defun render-scene (scene)
  (with-slots (view) scene
    (%fm:render-view *renderer* view)))


(defun transform-scene-camera (scene transform)
  (with-slots (camera) scene
    (%fm:with-mat4f (mat transform)
      (%fm:update-camera-model-matrix camera mat))))


(defun scene-camera-lens-projection (scene focal-length aspect near far)
  (with-slots (camera) scene
    (%fm:update-camera-lens-projection camera focal-length aspect near far)))


(defun scene-camera-ortho-projection (scene left right bottom top &key (near 0f0) (far 1f0))
  (with-slots (camera) scene
    (%fm:update-camera-projection camera :ortho left right bottom top near far)))


(defun add-scene-entity (scene entity)
  (with-slots (scene) scene
    (%fm:add-scene-entity scene entity)))


(defun remove-scene-entity (scene entity)
  (with-slots (scene) scene
    (%fm:remove-scene-entity scene entity)))


;;;
;;; SKYBOX
;;;
(defun make-color-skybox (r g b a)
  (%fm:with-vec4f (color r g b a)
    (%fm:with-skybox-builder (%make-skybox (:color color))
      (%make-skybox (handle-of *engine*)))))


(defun make-cubemap-skybox (cubemap)
  (%fm:with-skybox-builder (%make-skybox (:environment cubemap))
    (%make-skybox (handle-of *engine*))))


(defun destroy-skybox (skybox)
  (%fm:destroy-skybox (handle-of *engine*) skybox))
