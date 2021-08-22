(cl:in-package :alien-works.support.resources)


(defparameter *property-class-table* (make-hash-table :test #'equal))


(cffi:defcenum texture-mag-filter
  (:unset 0)
  (:nearest 9728)
  (:linear 9729))


(cffi:defcenum texture-min-filter
  (:unset 0)
  (:nearest 9728)
  (:linear 9729)
  (:nearest_mipmap_nearest 9984)
  (:linear_mipmap_nearest 9985)
  (:nearest_mipmap_linear 9986)
  (:linear_mipmap_linear 9987))


(u:define-enumval-extractor texture-type-enum %assimp:texture-type)
(u:define-enumval-extractor texture-map-mode-enum %assimp:texture-map-mode)
(u:define-enumval-extractor texture-mag-filter-enum texture-mag-filter)
(u:define-enumval-extractor texture-min-filter-enum texture-min-filter)


(defun register-material-property-class (name class)
  (setf (gethash name *property-class-table*) class))

(defun find-material-property-class (name)
  (gethash name *property-class-table*))


(defgeneric material-property-name (material))
(defgeneric init-material-property (property material))
(defgeneric apply-material-property (property material)
  (:method (property material)
    (declare (ignore material))
    (warn "~A is not applied" (material-property-name property))))


(defstruct (texture
            (:constructor %make-texture))
  name
  channel
  coord-id
  mapping-id
  mapping-name
  mapping-mode-u
  mapping-mode-v
  mapping-filter-mag
  mapping-filter-min
  uv-transform
  scale
  strength)


(defstruct (material
            (:constructor %make-material (id)))
  id
  name
  shininess
  two-sided
  ambient-color
  shininess-strength
  opacity
  reflectivity
  bump-scaling
  displacement-scaling
  diffuse-color
  specular-color
  emissive-color
  transparent-color
  reflective-color
  ;; metallic-roughness
  base-color-factor
  metallic-factor
  roughness-factor
  glossiness-factor

  alpha-mode
  alpha-cutoff
  (texture-table (make-hash-table :test #'equal)))


(defun material-texture (material type &optional (id 0) ensure-texture)
  (let* ((tex-table (material-texture-table material))
         (key (cons type id))
         (texture (gethash key tex-table)))
    (if (and (null texture) ensure-texture)
        (setf (gethash key tex-table) (%make-texture))
        texture)))


(defun (setf material-texture) (value material type &optional (id 0))
  (setf (gethash (cons type id) (material-texture-table material)) value))


;;;
;;; MATERIAL PROPERTIES
;;;
(defclass material-property ()
  ((name :initarg :name :initform (error ":name missing") :reader material-property-name)
   (type :initarg :type :initform (error ":type missing"))
   (index :initarg :index :initform (error ":index missing"))
   (value :initform nil :reader material-property-value)))


(defun invoke-material-setter (property material setter)
  (with-slots (value) property
    (funcall setter value material)))


(defmacro define-material-property ((class name type &optional setter))
  (a:with-gensyms (this material)
    `(progn
       (defclass ,class (,type) ())
       (register-material-property-class ,name ',class)
       ,@(when setter
           `((defmethod apply-material-property ((,this ,class) ,material)
               (invoke-material-setter ,this ,material #'(setf ,setter))))))))


(defun read-float-array-property (material name type index expected-size)
  (let ((result (make-simple-array expected-size 'single-float)))
    (cref:c-with ((actual-size :unsigned-int))
      (with-simple-array-pointer (ptr result)
        (%ai:get-material-float-array material name type index ptr (actual-size &)))
      (unless (= expected-size actual-size)
        (error "Unexpected property size: expected ~A, but got ~A" expected-size actual-size))
      result)))


(defun read-integer-array-property (material name type index expected-size)
  (let ((result (make-simple-array expected-size '(signed-byte 32))))
    (cref:c-with ((actual-size :unsigned-int))
      (with-simple-array-pointer (ptr result)
        (%ai:get-material-integer-array material name type index ptr (actual-size &)))
      (unless (= expected-size actual-size)
        (error "Unexpected property size: expected ~A, but got ~A" expected-size actual-size))
      result)))


(defun read-material-integer-property (material name type index)
  (cref:c-with ((value :int))
    (%ai:get-material-integer-array material name type index (value &) (cffi:null-pointer))
    value))


(defun read-material-float-property (material name type index)
  (cref:c-with ((value :float))
    (%ai:get-material-float-array material name type index (value &) (cffi:null-pointer))
    value))


;;;
;;; STRING MATERIAL PROPERTY
;;;
(defclass material-string-property (material-property) ())

(defmethod init-material-property ((this material-string-property) material)
  (with-slots (value name type index) this
    (cref:c-with ((str (:struct %ai:string)))
      (%ai:get-material-string material name type index (str &))
      (setf value (ai-string-to-lisp (str &))))))

;;;
;;; COLOR3 MATERIAL PROPERTY
;;;
(defclass material-color3-property (material-property) ())

(defmethod init-material-property ((this material-color3-property) material)
  (with-slots (value name type index) this
    (cref:c-with ((color (:struct %ai:color3d)))
      (%ai:get-material-color material name type index (color &))
      (setf value (make-array 3 :element-type 'single-float
                                :initial-contents (list (color :r) (color :g) (color :b)))))))

;;;
;;; VEC4 MATERIAL PROPERTY
;;;
(defclass material-vec4-property (material-property) ())

(defmethod init-material-property ((this material-vec4-property) material)
  (with-slots (value name type index) this
    (setf value (read-float-array-property material name type index 4))))

;;;
;;; FLOAT MATERIAL PROPERTY
;;;
(defclass material-float-property (material-property) ())

(defmethod init-material-property ((this material-float-property) material)
  (with-slots (value name type index) this
    (setf value (read-material-float-property material name type index))))

;;;
;;; BOOL MATERIAL PROPERTY
;;;
(defclass material-bool-property (material-property) ())


(defmethod init-material-property ((this material-bool-property) material)
  (with-slots (value name type index) this
    (setf value (/= (read-material-integer-property material name type index) 0))))

;;;
;;; INTEGER MATERIAL PROPERTY
;;;
(defclass material-integer-property (material-property) ())

(defmethod init-material-property ((this material-integer-property) material)
  (with-slots (value name type index) this
    (setf value (read-material-integer-property material name type index))))


;;;
;;; COMMON MATERIAL PROPERTIES
;;;
(define-material-property (material-name "?mat.name" material-string-property
                                         material-name))

(define-material-property (material-shininess "$mat.shininess" material-float-property
                                              material-shininess))

(define-material-property (material-two-sided "$mat.twosided" material-bool-property
                                              material-two-sided))
;;;
;;; COLOR MATERIAL PROPERTIES
;;;
(define-material-property (material-color-diffuse "$clr.diffuse" material-color3-property
                                                  material-diffuse-color))

(define-material-property (material-color-emissive "$clr.emissive" material-color3-property
                                                   material-emissive-color))

;;;
;;; TEXTURE
;;;
(defun apply-texture-property (property material setter)
  (with-slots (value index type) property
    (let ((tex (material-texture material (texture-type-enum type) index t)))
      (funcall setter value tex))))


(defmacro define-texture-property-applier ((property-type setter))
  (a:with-gensyms (this material)
    `(defmethod apply-material-property ((,this ,property-type) ,material)
       (apply-texture-property ,this ,material #'(setf ,setter)))))


(define-material-property (material-texture-file
                           "$tex.file"
                           material-string-property))
(defmethod apply-material-property ((this material-texture-file) material)
  (flet ((setter (value texture)
           (pushnew value *images* :test #'string=)
           (setf (texture-name texture) value)))
    (apply-texture-property this material #'setter)))


(define-material-property (material-texture-map-mode-u
                           "$tex.mapmodeu"
                           material-integer-property))
(defmethod apply-material-property ((this material-texture-map-mode-u) material)
  (flet ((setter (value texture)
           (setf (texture-mapping-mode-u texture) (texture-map-mode-enum value))))
    (apply-texture-property this material #'setter)))


(define-material-property (material-texture-map-mode-v
                           "$tex.mapmodev"
                           material-integer-property))
(defmethod apply-material-property ((this material-texture-map-mode-v) material)
  (flet ((setter (value texture)
           (setf (texture-mapping-mode-v texture) (texture-map-mode-enum value))))
    (apply-texture-property this material #'setter)))


(define-material-property (material-texture-uvwsrc
                           "$tex.uvwsrc"
                           material-string-property))
(define-texture-property-applier (material-texture-uvwsrc
                                  texture-channel))


(define-material-property (material-texture-tex-coord
                           "$tex.file.texCoord"
                           material-integer-property))
(define-texture-property-applier (material-texture-tex-coord
                                  texture-coord-id))


(define-material-property (material-texture-mapping-name
                           "$tex.mappingname"
                           material-string-property))
(define-texture-property-applier (material-texture-mapping-name
                                  texture-mapping-name))


(define-material-property (material-texture-mapping-id
                           "$tex.mappingid"
                           material-string-property))
(define-texture-property-applier (material-texture-mapping-id
                                  texture-mapping-id))


(define-material-property (material-texture-mapping-filter-mag
                           "$tex.mappingfiltermag"
                           material-integer-property))
(defmethod apply-material-property ((this material-texture-mapping-filter-mag) material)
  (flet ((setter (value texture)
           (setf (texture-mapping-filter-mag texture) (texture-mag-filter-enum value))))
    (apply-texture-property this material #'setter)))


(define-material-property (material-texture-mapping-filter-min
                           "$tex.mappingfiltermin"
                           material-integer-property))
(defmethod apply-material-property ((this material-texture-mapping-filter-min) material)
  (flet ((setter (value texture)
           (setf (texture-mapping-filter-min texture) (texture-min-filter-enum value))))
    (apply-texture-property this material #'setter)))


(define-material-property (material-texture-scale
                           "$tex.scale"
                           material-float-property))
(define-texture-property-applier (material-texture-scale
                                  texture-scale))


(define-material-property (material-texture-strength
                           "$tex.strength"
                           material-float-property))
(define-texture-property-applier (material-texture-strength
                                  texture-strength))



;;;
;;; PBR MATERIAL PROPERTIES
;;;
(define-material-property (material-pbr-metallic-roughness-base-color-factor
                           "$mat.gltf.pbrMetallicRoughness.baseColorFactor"
                           material-vec4-property
                           material-base-color-factor))

(define-material-property (material-pbr-metallic-roughness-metallic-factor
                           "$mat.gltf.pbrMetallicRoughness.metallicFactor"
                           material-float-property
                           material-metallic-factor))

(define-material-property (material-pbr-metallic-roughness-roughness-factor
                           "$mat.gltf.pbrMetallicRoughness.roughnessFactor"
                           material-float-property
                           material-roughness-factor))

(define-material-property (material-pbr-metallic-roughness-glossiness-factor
                           "$mat.gltf.pbrMetallicRoughness.glossinessFactor"
                           material-float-property
                           material-glossiness-factor))

(define-material-property (material-pbr-alpha-mode
                           "$mat.gltf.alphaMode"
                           material-string-property))

(defmethod apply-material-property ((this material-pbr-alpha-mode) material)
  (with-slots (value) this
    (let ((mode (a:eswitch (value :test #'equalp)
                  ("opaque" :opaque)
                  ("mask" :mask)
                  ("blend" :blend))))
      (setf (material-alpha-mode material) mode))))


(define-material-property (material-pbr-alpha-cutoff
                           "$mat.gltf.alphaCutoff"
                           material-float-property
                           material-alpha-cutoff))

(define-material-property (material-pbr-specular-glossiness
                           "$mat.gltf.pbrSpecularGlossiness"
                           material-bool-property))

(define-material-property (material-pbr-unlit
                           "$mat.gltf.unlit"
                           material-bool-property))


(defun set-material-property (parsed-material material property)
  (with-material-property (property property)
    (let ((property-name (ai-string-to-lisp (property :key &))))
      (a:if-let ((property-class (find-material-property-class property-name)))
        (let ((instance (make-instance property-class :name property-name
                                                      :type (property :semantic)
                                                      :index (property :index))))
          (init-material-property instance material)
          (apply-material-property instance parsed-material))
        (warn "Ignoring property ~A" property-name)))))


(defun parse-material (id material)
  (with-material (material material)
    (loop with parsed = (%make-material id)
          for i from 0 below (material :num-properties)
          do (set-material-property parsed (material &) (material :properties * i))
          finally (return parsed))))


(defun parse-materials ()
  (with-scene (scene)
    (loop for i from 0 below (scene :num-materials)
          collect (parse-material i (scene :materials * i)))))
