(cl:in-package :alien-works.graphics)


(defvar *material-registry* (make-hash-table))


(defun register-material (name value)
  (setf (gethash name *material-registry*) value))


(defun find-material (name)
  (gethash name *material-registry*))


(defclass material ()
  ((name :initarg :name)
   (parameters :initform nil)
   (requires :initform nil)
   (variables :initform nil)
   (vertex-shader :initform nil
                  :reader material-vertex-shader-name)
   (fragment-shader :initform nil
                    :reader material-fragment-shader-name)
   (shading-model :initarg :shading-model
                  :initform nil)
   (variant-filter :initarg :variant-filter
                   :initform nil)
   (flip-uv :initarg :flip-uv
            :initform nil)
   (quality :initarg :quality
            :initform nil)
   (instanced :initarg :instanced
              :initform nil)
   (vertex-domain :initarg :vertex-domain
                  :initform nil)
   (interpolation :initarg :interpolation
                  :initform nil)
   (blending :initarg :blending
             :initform nil)
   (post-lighting-blending :initarg :post-lighting-blending
                           :initform nil)
   (transparency :initarg :transparency
                 :initform nil)
   (mask-threshold :initarg :mask-threshold
                   :initform nil)
   (refraction-mode :initarg :refraction-mode
                    :initform nil)
   (refraction-type :initarg :refraction-type
                    :initform nil)
   (culling :initarg :culling
            :initform nil)
   (color-write :initarg :color-write
                :initform nil)
   (depth-write :initarg :depth-write
                :initform nil)
   (depth-culling :initarg :depth-culling
                  :initform nil)
   (double-sided :initarg :double-sided
                 :initform nil)
   (reflections :initarg :reflections
                :initform nil)
   (shadow-multiplier :initarg :shadow-multiplier
                      :initform nil)
   (transparent-shadow :initarg :transparent-shadow
                       :initform nil)
   (clear-coat-ior-change :initarg :clear-coat-ior-change
                          :initform nil)
   (multi-bounce-ambient-occlusion :initarg :multi-bounce-ambient-occlusion
                                   :initform nil)
   (specular-ambient-occlusion :initarg :specular-ambient-occlusion
                               :initform nil)
   (specular-anti-aliasing :initarg :specular-anti-aliasing
                           :initform nil)
   (specular-anti-aliasing-variance :initarg :specular-anti-aliasing-variance
                                    :initform nil)
   (specular-anti-aliasing-threshold :initarg :specular-anti-aliasing-threshold
                                     :initform nil)
   (custom-surface-shading :initarg :custom-surface-shading
                           :initform nil)))


(defun update-material-shader (material-name stage shader-name
                               shader-parameters
                               shader-attributes
                               shader-variables)
  (a:if-let ((material (find-material material-name)))
    (with-slots (parameters requires variables fragment-shader vertex-shader) material
      (setf (a:assoc-value parameters stage) shader-parameters
            (a:assoc-value requires stage) shader-attributes
            (a:assoc-value variables stage) shader-variables)
      (ecase stage
        (:vertex (setf vertex-shader shader-name))
        (:fragment (setf fragment-shader shader-name))))
    (error "Material ~A not found" material-name)))


(defun update-material-vertex-shader (material-name stage parameters)
  (a:when-let ((material (find-material material-name)))
    (setf (a:assoc-value (slot-value material 'parameters) stage) parameters)))


(defun format-material-descriptor (material stream)
  (format stream "material {")

  (macrolet ((with-property ((string-name slot-name &optional processing) &body body)
               `(a:when-let* ((,slot-name (slot-value material ',slot-name))
                              ,@(when processing `((,slot-name ,processing))))
                  (format stream ",~&  ~A : " ,string-name)
                  ,@body))
             (format-boolean-property (string-name slot-name)
               `(with-property (,string-name ,slot-name)
                  (format stream "~A" (if (first ,slot-name) "true" "false"))))
             (format-switch-property (string-name slot-name
                                      &rest switches
                                      &key &allow-other-keys)
               `(with-property (,string-name ,slot-name)
                  (format stream "~A" (ecase ,slot-name
                                        ,@(loop :for (n . v) :in (a:plist-alist switches)
                                                :collect `(,n ,v)))))))

    (format stream "~&  name : \"~A\"" (slot-value material 'name))

    (format-switch-property "shadingModel" shading-model
                            :lit "lit"
                            :subsurface "subsurface"
                            :cloth "cloth"
                            :unlit "unlit"
                            :specular-glossiness "specularGlossiness")

    (with-property ("parameters" parameters
                                 (remove-duplicates
                                  (apply #'append (mapcar #'cdr parameters))
                                  :key #'first))
      (format stream "[")
      (loop for (parameter . rest-parameters) on parameters
            do (destructuring-bind (name type &key format precision) parameter
                 (flet ((type->string ()
                          (ecase type
                            (:bool "bool")
                            (:bvec2 "bool2")
                            (:bvec3 "bool3")
                            (:bvec4 "bool4")

                            (:float "float")
                            (:vec2 "float2")
                            (:vec3 "float3")
                            (:vec4 "float4")

                            (:int "int")
                            (:ivec2 "int2")
                            (:ivec3 "int3")
                            (:ivec4 "int4")

                            (:uint "uint")
                            (:uvec2 "uint2")
                            (:uvec3 "uint3")
                            (:uvec4 "uint4")

                            (:mat3 "float3x3")
                            (:mat4 "float4x4")

                            (:sampler-2d "sampler2d")
                            (:sampler-2d-array "sampler2dArray")
                            (:sampler-external "samplerExternal")
                            (:sampler-cubemap "samplerCubemap")))
                        (format->string ()
                          (ecase format
                            (:int "int")
                            (:float "float")))
                        (precision->string ()
                          (ecase precision
                            (:low "low")
                            (:medium "medium")
                            (:high "high")
                            (:default "default")))
                        (name->string ()
                          (varjo.internals:safe-glsl-name-string name)))
                   (progn
                     (format stream "~&    {")
                     (format stream "~&      name : ~A," (name->string))
                     (format stream "~&      type : ~A," (type->string)))
                   (when format
                     (format stream "~&      format : ~A," (format->string)))
                   (when precision
                     (format stream "~&      precision : ~A," (precision->string)))
                   (progn
                     (format stream "~&    }")
                     (when rest-parameters
                       (format stream ","))))))
      (format stream "~&  ]"))

    (with-property ("variantFilter" variant-filter)
      (format stream "[ ~{~A~^, ~} ]"
              (loop for variant in variant-filter
                    collect (ecase variant
                              (:directional-lighting "directionalLighting")
                              (:dynamic-lighting "dynamicLighting")
                              (:shadow-receiver "shadowReceiver")
                              (:skinning "skinning")
                              (:fog "fog")
                              (:vsm "vsm")
                              (:ssr "ssr")))))

    (format-boolean-property "flipUV" flip-uv)

    (with-property ("instanced" instanced)
      (format stream "true"))

    (with-property ("requires" requires
                               (remove-duplicates
                                (apply #'append (mapcar #'cdr requires))))
      (format stream "[ ~{~A~^, ~} ]"
              (loop for require in requires
                    collect (ecase require
                              (:uv0 "uv0")
                              (:uv1 "uv1")
                              (:color "color")
                              (:position "position")
                              (:tangents "tangents")
                              (:custom0 "custom0")
                              (:custom1 "custom1")
                              (:custom2 "custom2")
                              (:custom3 "custom3")
                              (:custom4 "custom4")
                              (:custom5 "custom5")
                              (:custom6 "custom6")
                              (:custom7 "custom7")))))

    (with-property ("variables" variables
                                (mapcar #'varjo.internals:safe-glsl-name-string
                                        (remove-duplicates
                                         (apply #'append (mapcar #'cdr variables)))))
      (format stream "[ ~{~A~^, ~} ]" variables))


    (with-property ("vertexDomain" vertex-domain)
      (format stream "~A" (ecase vertex-domain
                            (:object "object")
                            (:world "world")
                            (:view "view")
                            (:device "device"))))

    (with-property ("interpolation" interpolation)
      (format stream "~A" (ecase interpolation
                            (:smooth "smooth")
                            (:flat "flat"))))

    (with-property ("blending" blending)
      (format stream "~A" (ecase blending
                            (:opaque "opaque")
                            (:transparent "transparent")
                            (:fade "fade")
                            (:add "add")
                            (:masked "masked")
                            (:multiply "multiply")
                            (:screen "screen"))))

    (with-property ("postLightingBlending" post-lighting-blending)
      (format stream "~A" (ecase post-lighting-blending
                            (:opaque "opaque")
                            (:transparent "transparent")
                            (:add "add"))))


    (with-property ("transparency" transparency)
      (format stream "~A" (ecase transparency
                            (:default "default")
                            (:two-passes-one-side "twoPassesOneSide")
                            (:two-passes-two-sides "twoPassesTwoSides"))))

    (with-property ("maskThreshold" mask-threshold)
      (format stream "~F" mask-threshold))

    (with-property ("refractionMode" refraction-mode)
      (format stream "~A" (ecase refraction-mode
                            (:none "none")
                            (:cubemap "cubemap")
                            (:screenspace "screenspace"))))

    (with-property ("refractionType" refraction-type)
      (format stream "~A" (ecase refraction-type
                            (:solid "solid")
                            (:thin "thin"))))

    (with-property ("culling" culling)
      (format stream "~A" (ecase culling
                            (:none "none")
                            (:front "front")
                            (:back "back")
                            (:front-and-back "frontAndBack"))))

    (format-boolean-property "colorWrite" color-write)

    (format-boolean-property "depthWrite" depth-write)

    (format-boolean-property "depthCulling" depth-culling)

    (format-boolean-property "doubleSided" double-sided)

    (with-property ("reflections" reflections)
      (format stream "~A" (ecase reflections
                            (:default "default")
                            (:screenspace "screenspace"))))

    (format-boolean-property "shadowMultiplier" shadow-multiplier)

    (format-boolean-property "transparentShadow" transparent-shadow)

    (format-boolean-property "clearCoatIorChange" clear-coat-ior-change)

    (format-boolean-property "multiBounceAmbientOcclusion" multi-bounce-ambient-occlusion)

    (with-property ("specularAmbientOcclusion" specular-ambient-occlusion)
      (format stream "~A" (ecase specular-ambient-occlusion
                            (:none "none")
                            (:simple "simple")
                            (:bent-normals "bentNormals"))))

    (format-boolean-property "specularAntiAliasing" specular-anti-aliasing)

    (with-property ("specularAntiAliasingVariance" specular-anti-aliasing-variance)
      (format stream "~F" specular-anti-aliasing-variance))

    (with-property ("specularAntiAliasingThreshold" specular-anti-aliasing-threshold)
      (format stream "~F" specular-anti-aliasing-threshold))

    (format-boolean-property "customSurfaceShading" custom-surface-shading)
    (format stream "~&}")))


(defun symbolicate-material-name (name)
  (u:symbolicate* name '$alien-works$material))


;; inheritance?
(defmacro defmaterial (name-and-opts () &body configuration)
  (destructuring-bind (name) (a:ensure-list name-and-opts)
    (destructuring-bind (&key shading-model
                           variant-filter
                           (flip-uv t flip-uv-provided-p)
                           quality
                           instanced
                           vertex-domain
                           interpolation
                           blending
                           post-lighting-blending
                           transparency
                           mask-threshold
                           refraction-mode
                           refraction-type
                           culling
                           color-write
                           depth-write
                           depth-culling
                           double-sided
                           reflections
                           shadow-multiplier
                           transparent-shadow
                           clear-coat-ior-change
                           multi-bounce-ambient-occlusion
                           specular-ambient-occlusion
                           specular-anti-aliasing
                           specular-anti-aliasing-variance
                           specular-anti-aliasing-threshold
                           custom-surface-shading)
        (a:alist-plist configuration)
      (let ((material-name (symbolicate-material-name name)))
        `(progn
           (defclass ,material-name (material)
             ()
             (:default-initargs
              :name ,(format nil "~(~A::~A~)"
                             (package-name (symbol-package name))
                             (symbol-name name))
              ,@(when shading-model
                  `(:shading-model ,@shading-model))
              ,@(when variant-filter
                  `(:variant-filter ,@variant-filter))
              ,@(when flip-uv-provided-p
                  `(:flip-uv ',flip-uv))
              ,@(when quality
                  `(:quality ,@quality))
              ,@(when instanced
                  `(:instanced ,@instanced))
              ,@(when vertex-domain
                  `(:vertex-domain ,@vertex-domain))
              ,@(when interpolation
                  `(:interpolation ,@interpolation))
              ,@(when blending
                  `(:blending ,@blending))
              ,@(when post-lighting-blending
                  `(:post-lighting-blending ,@post-lighting-blending))
              ,@(when transparency
                  `(:transparency ,@transparency))
              ,@(when mask-threshold
                  `(:mask-threshold ,@mask-threshold))
              ,@(when refraction-mode
                  `(:refraction-mode ,@refraction-mode))
              ,@(when refraction-type
                  `(:refraction-type ,@refraction-type))
              ,@(when culling
                  `(:culling ,@culling))
              ,@(when color-write
                  `(:color-write ',color-write))
              ,@(when depth-write
                  `(:depth-write ',depth-write))
              ,@(when depth-culling
                  `(:depth-culling ',depth-culling))
              ,@(when double-sided
                  `(:double-sided ',double-sided))
              ,@(when reflections
                  `(:reflections ,@reflections))
              ,@(when shadow-multiplier
                  `(:shadow-multiplier ',shadow-multiplier))
              ,@(when transparent-shadow
                  `(:transparent-shadow ',transparent-shadow))
              ,@(when clear-coat-ior-change
                  `(:clear-coat-ior-change ',clear-coat-ior-change))
              ,@(when multi-bounce-ambient-occlusion
                  `(:multi-bounce-ambient-occlusion ',multi-bounce-ambient-occlusion))
              ,@(when specular-ambient-occlusion
                  `(:specular-ambient-occlusion ,@specular-ambient-occlusion))
              ,@(when specular-anti-aliasing
                  `(:specular-anti-aliasing ',specular-anti-aliasing))
              ,@(when specular-anti-aliasing-variance
                  `(:specular-anti-aliasing-variance ,@specular-anti-aliasing-variance))
              ,@(when specular-anti-aliasing-threshold
                  `(:specular-anti-aliasing-threshold ,@specular-anti-aliasing-threshold))
              ,@(when custom-surface-shading
                  `(:custom-surface-shading ',custom-surface-shading))))
           (register-material ',name (make-instance ',material-name)))))))


(varjo:define-vari-struct |MaterialInputs| (:330)
  ;; default: float4(1.0)
  (|baseColor| :vec4 :accessor fragment-base-color)
  ;; default: float4(0.0, 0.0, 0.0, 1.0)
  (|emissive| :vec4 :accessor fragment-emissive-color)
  ;; default: float4(0.0)
  (|postLightingColor| :vec4 :accessor fragment-post-lighting-color)

  ;; no other field is available with the unlit shading model
  ;; default: 1.0
  (|roughness| :float :accessor fragment-roughness)
  ;; default: 0.0, not available with cloth or specularGlossiness
  (|metallic| :float :accessor fragment-metallic)
  ;; default: 0.5, not available with cloth or specularGlossiness
  (|reflectance| :float :accessor fragment-reflectance)
  ;; default: 0.0
  (|ambientOcclusion| :float :accessor fragment-ambient-occlusion)

  ;; not available when the shading model is subsurface or cloth
  ;; default: float3(0.0)
  (|sheenColor| :vec3 :accessor fragment-sheen-color)
  ;; default: 0.0
  (|sheenRoughness| :float :accessor fragment-sheen-roughness)
  ;; default: 1.0
  (|clearCoat| :float :accessor fragment-clear-coat)
  ;; default: 0.0
  (|clearCoatRoughness| :float :accessor fragment-clear-coat-roughness)
  ;; default: float3(0.0, 0.0, 1.0)
  (|clearCoatNormal| :vec3 :accessor fragment-coat-normal)
  ;; default: 0.0
  (|anisotropy| :float :accessor fragment-anisotropy)
  ;; default: float3(1.0, 0.0, 0.0)
  (|anisotropyDirection| :vec3 :accessor fragment-anisotropy-direction)

  ;; only available when the shading model is subsurface or refraction is enabled
  ;; default: 0.5
  (|thickness| :float :accessor fragment-thickness)

  ;; only available when the shading model is subsurface
  ;; default: 12.234
  (|subsurfacePower| :float :accessor fragment-subsurface-power)
  ;; default: float3(1.0)
  (|subsurfaceColor| :vec3 :accessor fragment-subsurface-color)

  ;; only available when the shading model is cloth
  ;; default: sqrt(baseColor)
  (|sheenColor| :vec3 :accessor fragment-sheen-color)
  ;; default: float3(0.0)
  (|subsurfaceColor| :vec3 :accessor fragment-subsurface-color)

  ;; only available when the shading model is specularGlossiness
  ;; default: float3(0.0)
  (|specularColor| :vec3 :accessor fragment-specular-color)
  ;; default: 0.0
  (|glossiness| :float :accessor fragment-glossiness)

  ;; not available when the shading model is unlit
  ;; must be set before calling prepareMaterial()
  ;; default: float3(0.0, 0.0, 1.0)
  (|normal| :vec3 :accessor fragment-normal)

  ;; only available when refraction is enabled
  ;; default: 1.0
  (|transmission| :float :accessor fragment-transmission)
  ;; default float3(0.0, 0.0, 0.0)
  (|absorption| :vec3 :accessor fragment-absorption)
  ;; default: 1.5
  (|microThickness| :float :accessor fragment-micro-thickness)
  ;; default: 0.0, not available with refractionType "solid"
  (|ior| :float :accessor fragment-ior))


(defun symbolicate-fragment-shader-name (material-name)
  (u:symbolicate* material-name '$alien-works$fragment))


(defun parse-shader-input (input)
  (loop with arg-alist = (list)
        with input-kind = :param
        for input-def in input
        if (and (atom input-def)
                (member input-def '(&attribute &variable) :test #'string=))
          do (setf input-kind
                   (a:eswitch (input-def :test #'string=)
                     ('&attribute :attrib)
                     ('&variable :var)))
        else
          do (push (a:ensure-list input-def) (a:assoc-value arg-alist input-kind))
        finally (return (values (nreverse (a:assoc-value arg-alist :param))
                                (nreverse (a:assoc-value arg-alist :attrib))
                                (nreverse (a:assoc-value arg-alist :var))))))


(defun expand-shader-parameters (params)
  (loop :for (name type) :in params
        :collect (let ((glsl-name
                         (varjo.internals:safe-glsl-name-string name)))
                   `(,name
                     ,(case type
                        ((:sampler-2d
                          :sampler-2d-array
                          :sampler-external
                          :sampler-cubemap)
                         (format nil "materialParams_~A" glsl-name))
                        (t (format nil "materialParams.~A" glsl-name)))
                     ,type
                     :read-only t))))


(defmacro define-fragment-shader (((material-fragment material-name) &rest input)
                                  &body body)
  (let ((shader-name (symbolicate-fragment-shader-name material-name)))
    (multiple-value-bind (parameters attributes variables)
        (parse-shader-input input)
      (let ((params (expand-shader-parameters parameters))
            (attribs (loop :for (name kind) :in attributes
                           :collect `(,name
                                      ,(ecase kind
                                         (:uv0 '(get-uv0))
                                         (:uv1 '(get-uv1))
                                         (:color '(get-color))
                                         (:tangents '(get-tangents))))))
            (vars (loop :for (name) :in variables
                        :collect `(,name
                                   ,(format nil "variable_~A"
                                            (varjo.internals:safe-glsl-name-string name))
                                   :vec4
                                   :read-only t))))
        `(progn
           (varjo:define-vari-function ,shader-name
               ((,material-fragment |MaterialInputs| :in/out))
             (symbol-macrolet (,@attribs)
               (%varjo:letvar (,@params ,@vars) ,@body))
             (values))
           (update-material-shader ',material-name
                                   :fragment
                                   ',shader-name
                                   ',parameters
                                   ',(mapcar #'second attributes)
                                   ',(mapcar #'first variables)))))))


(varjo:define-vari-struct |MaterialVertexInputs| (:330)
  ;; if the color attribute is required
  (|color| :vec4 :accessor vertex-color)
  ;; if the uv0 attribute is required
  (|uv0| :vec2 :accessor vertex-uv0)
  ;; if the uv1 attribute is required
  (|uv1| :vec2 :accessor vertex-uv1)
  ;; only if the shading model is not unlit
  (|worldNormal| :vec3 :accessor vertex-world-normal)
  ;; always available (see note below about world-space)
  (|worldPosition| :vec4 :accessor vertex-world-position)

  ;; default: identity, transforms the clip-space position
  (|clipSpaceTransform| :mat4 :accessor vertex-clip-space-transform)

  ;; World Position
  ;; To achieve good precision, the worldPosition coordinate in
  ;; the vertex shader is shifted by the camera position. To get the true
  ;; world-space position, users can add this to getWorldOffset().

  ;; UV attributes
  ;; By default the vertex shader of a material will flip the Y
  ;; coordinate of the UV attributes of the current mesh: material.uv0 =
  ;; vec2(mesh_uv0.x, 1.0 - mesh_uv0.y). You can control this behavior using the
  ;; flipUV property and setting it to false.
  )

(defun symbolicate-vertex-shader-name (material-name)
  (u:symbolicate* material-name '$alien-works$vertex))


(defmacro define-vertex-shader (((material-vertex material-name) &rest input)
                                &body body)
  (let ((shader-name (symbolicate-vertex-shader-name material-name)))
    (multiple-value-bind (parameters attributes variables)
        (parse-shader-input input)
      (let ((params (expand-shader-parameters parameters))
            (attribs (loop :for (name kind) :in attributes
                           :collect `(,name
                                      ,(ecase kind
                                         (:uv0 `(vertex-uv0 ,material-vertex))
                                         (:uv1 `(vertex-uv1 ,material-vertex))
                                         (:color `(vertex-color ,material-vertex))
                                         (:position '(get-position))
                                         (:tangents '(get-tangents))
                                         (:custom0 '(get-custom0))
                                         (:custom1 '(get-custom1))
                                         (:custom2 '(get-custom2))
                                         (:custom3 '(get-custom3))
                                         (:custom4 '(get-custom4))
                                         (:custom5 '(get-custom5))
                                         (:custom6 '(get-custom6))
                                         (:custom7 '(get-custom7))))))
            (vars (loop :for (name) :in variables
                        :collect `(,name
                                   ,(format nil "~A.~A"
                                            (varjo.internals:safe-glsl-name-string
                                             material-vertex)
                                            (varjo.internals:safe-glsl-name-string
                                             name))
                                   :vec4))))
        `(progn
           (varjo:define-vari-function ,shader-name
               ((,material-vertex |MaterialVertexInputs| :in/out))
             (symbol-macrolet (,@attribs)
               (%varjo:letvar (,@params ,@vars) ,@body))
             (values))
           (update-material-shader ',material-name
                                   :vertex
                                   ',shader-name
                                   ',parameters
                                   ',(mapcar #'second attributes)
                                   ',(mapcar #'first variables)))))))


(varjo:define-glsl-template-function get-uv0 () :vec2
  "getUV0()"
  :pure t)
(varjo:define-glsl-template-function get-uv1 () :vec2
  "getUV1()"
  :pure t)
(varjo:define-glsl-template-function get-color () :vec4
  "getColor()"
  :pure t)
(varjo:define-glsl-template-function get-position () :vec4
    "getPosition()"
  :pure t)
(varjo:define-glsl-template-function get-tangents () :vec4
    "getTangents()"
  :pure t)
(varjo:define-glsl-template-function get-custom0 () :vec4
    "getCustom0()"
  :pure t)
(varjo:define-glsl-template-function get-custom1 () :vec4
    "getCustom1()"
  :pure t)
(varjo:define-glsl-template-function get-custom2 () :vec4
    "getCustom2()"
  :pure t)
(varjo:define-glsl-template-function get-custom3 () :vec4
    "getCustom3()"
  :pure t)
(varjo:define-glsl-template-function get-custom4 () :vec4
    "getCustom4()"
  :pure t)
(varjo:define-glsl-template-function get-custom5 () :vec4
    "getCustom5()"
  :pure t)
(varjo:define-glsl-template-function get-custom6 () :vec4
    "getCustom6()"
  :pure t)
(varjo:define-glsl-template-function get-custom7 () :vec4
    "getCustom7()"
  :pure t)


(varjo:define-glsl-template-function prepare-material
    ((material-inputs |MaterialInputs|)) :void
  "prepareMaterial(~A)")


(varjo:define-vari-function default-fragment
    ((material-inputs |MaterialInputs| :in/out))
  (prepare-material material-inputs)
  (values))


(defun %graphics:print-material-source (material-name stream)
  (let ((material (find-material material-name)))
    (unless material
      (error "Material ~A not found" material-name))
    (format-material-descriptor material stream)

    (a:when-let (shader-name (material-vertex-shader-name material))
      (format stream "~&~%vertex {~%")
      (format stream (%varjo:format-glsl shader-name :vertex))
      (format stream "~&~%}"))

    (format stream "~&~%fragment {~%")
    (a:if-let (shader-name (material-fragment-shader-name material))
      (format stream (%varjo:format-glsl shader-name :fragment))
      (format stream (%varjo:format-glsl 'default-fragment :fragment)))
    (format stream "~&~%}")))


(defmethod print-object ((this material) stream)
  (print-unreadable-object (this stream :type t)
    (format-material-descriptor this stream)))
