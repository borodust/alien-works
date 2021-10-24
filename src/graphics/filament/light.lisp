(cl:in-package :%alien-works.filament)


;;;
;;; DIRECT
;;;
(u:define-enumval-extractor light-type-enum %filament:light-manager+type)

(warp-intricate-builder-option light-builder :cast-shadows
    %filament:cast-shadows
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':bool)

(warp-intricate-builder-option light-builder :shadow-options
    %filament:shadow-options
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  '(claw-utils:claw-pointer %filament:light-manager+shadow-options))

(warp-intricate-builder-option light-builder :cast-light
    %filament:cast-light
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':bool)

(warp-intricate-builder-option light-builder :position
    %filament:position
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  '(claw-utils:claw-pointer %filament:math+float3))

(warp-intricate-builder-option light-builder :direction
    %filament:direction
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  '(claw-utils:claw-pointer %filament:math+float3))

(warp-intricate-builder-option light-builder :color
    %filament:color
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  '(claw-utils:claw-pointer %filament:linear-color))

(warp-intricate-builder-option light-builder :intensity
    %filament:intensity-candela
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':float)

(warp-intricate-builder-option light-builder :intensity-efficiency
    %filament:intensity
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':float
  ':float)

(warp-intricate-builder-option light-builder :falloff
    %filament:falloff
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':float)

(warp-intricate-builder-option light-builder :spot-light-cone
    %filament:spot-light-cone
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':float
  ':float)

(warp-intricate-builder-option light-builder :sun-angular-radius
    %filament:sun-angular-radius
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':float)

(warp-intricate-builder-option light-builder :sun-halo-size
    %filament:sun-halo-size
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':float)

(warp-intricate-builder-option light-builder :sun-halo-falloff
    %filament:sun-halo-falloff
  '(claw-utils:claw-pointer %filament:light-manager+builder)
  ':float)


(defmacro with-light-builder ((name (type) &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:light-manager+builder '%filament:light-manager+type
                                                      (light-type-enum ,type)))
         (build-expander (builder)
           `(%filament:build
             '(claw-utils:claw-pointer %filament:light-manager+builder) ,builder
             '(claw-utils:claw-pointer %filament:engine) !::engine
             '(claw-utils:claw-pointer %filament:utils+entity) !::entity)))
    (explode-builder name
                     'light-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine !::entity)
                     steps
                     body)))

;;;
;;; INDIRECT
;;;
(warp-intricate-builder-option indirect-light :reflections
    %filament:reflections
  '(claw-utils:claw-pointer %filament:indirect-light+builder)
  '(claw-utils:claw-pointer %filament:texture))

(warp-intricate-builder-option indirect-light :radiance
    %filament:radiance
  '(claw-utils:claw-pointer %filament:indirect-light+builder)
  '%filament:uint8-t
  '(claw-utils:claw-pointer %filament:math+float3))

(warp-intricate-builder-option indirect-light :irradiance
    %filament:irradiance
  '(claw-utils:claw-pointer %filament:indirect-light+builder)
  '%filament:uint8-t
  '(claw-utils:claw-pointer %filament:math+float3))

(warp-intricate-builder-option indirect-light :cubemap-irradiance
    %filament:irradiance
  '(claw-utils:claw-pointer %filament:indirect-light+builder)
  '(claw-utils:claw-pointer %filament:texture))

(warp-intricate-builder-option indirect-light :intensity
    %filament:intensity
  '(claw-utils:claw-pointer %filament:indirect-light+builder)
  ':float)

(warp-intricate-builder-option indirect-light :rotation
    %filament:rotation
  '(claw-utils:claw-pointer %filament:indirect-light+builder)
  '(claw-utils:claw-pointer %filament:math+mat3f))

(defmacro with-indirect-light-builder ((name &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:indirect-light+builder))
         (build-expander (builder)
           `(%filament:build
             '(claw-utils:claw-pointer %filament:indirect-light+builder) ,builder
             '(claw-utils:claw-pointer %filament:engine) !::engine)))
    (explode-builder name
                     'indirect-light
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-indirect-light (engine light)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::indirect-light) light))
