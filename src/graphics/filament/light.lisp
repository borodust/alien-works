(cl:in-package :%alien-works.filament)


;;;
;;; DIRECT
;;;
(u:define-enumval-extractor light-type-enum %filament:filament+light-manager+type)

(warp-intricate-builder-option light-builder :cast-shadows
    %filament:filament+cast-shadows
  '(:pointer %filament:filament+light-manager+builder)
  ':bool)

(warp-intricate-builder-option light-builder :shadow-options
    %filament:filament+shadow-options
  '(:pointer %filament:filament+light-manager+builder)
  '(:pointer %filament:filament+light-manager+shadow-options))

(warp-intricate-builder-option light-builder :cast-light
    %filament:filament+cast-light
  '(:pointer %filament:filament+light-manager+builder)
  ':bool)

(warp-intricate-builder-option light-builder :position
    %filament:filament+position
  '(:pointer %filament:filament+light-manager+builder)
  '(:pointer %filament:filament+math+float3))

(warp-intricate-builder-option light-builder :direction
    %filament:filament+direction
  '(:pointer %filament:filament+light-manager+builder)
  '(:pointer %filament:filament+math+float3))

(warp-intricate-builder-option light-builder :color
    %filament:filament+color
  '(:pointer %filament:filament+light-manager+builder)
  '(:pointer %filament:filament+linear-color))

(warp-intricate-builder-option light-builder :intensity
    %filament:filament+intensity-candela
  '(:pointer %filament:filament+light-manager+builder)
  ':float)

(warp-intricate-builder-option light-builder :intensity-efficiency
    %filament:filament+intensity
  '(:pointer %filament:filament+light-manager+builder)
  ':float
  ':float)

(warp-intricate-builder-option light-builder :falloff
    %filament:filament+falloff
  '(:pointer %filament:filament+light-manager+builder)
  ':float)

(warp-intricate-builder-option light-builder :spot-light-cone
    %filament:filament+spot-light-cone
  '(:pointer %filament:filament+light-manager+builder)
  ':float
  ':float)

(warp-intricate-builder-option light-builder :sun-angular-radius
    %filament:filament+sun-angular-radius
  '(:pointer %filament:filament+light-manager+builder)
  ':float)

(warp-intricate-builder-option light-builder :sun-halo-size
    %filament:filament+sun-halo-size
  '(:pointer %filament:filament+light-manager+builder)
  ':float)

(warp-intricate-builder-option light-builder :sun-halo-falloff
    %filament:filament+sun-halo-falloff
  '(:pointer %filament:filament+light-manager+builder)
  ':float)


(defmacro with-light-builder ((name (type) &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:filament+light-manager+builder '%filament:filament+light-manager+type
                                                      (light-type-enum ,type)))
         (build-expander (builder)
           `(%filament:filament+build
             '(:pointer %filament:filament+light-manager+builder) ,builder
             '(:pointer %filament:filament+engine) !::engine
             '(:pointer %filament:utils+entity) !::entity)))
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
    %filament:filament+reflections
  '(:pointer %filament:filament+indirect-light+builder)
  '(:pointer %filament:filament+texture))

(warp-intricate-builder-option indirect-light :radiance
    %filament:filament+radiance
  '(:pointer %filament:filament+indirect-light+builder)
  '%filament:uint8-t
  '(:pointer %filament:filament+math+float3))

(warp-intricate-builder-option indirect-light :irradiance
    %filament:filament+irradiance
  '(:pointer %filament:filament+indirect-light+builder)
  '%filament:uint8-t
  '(:pointer %filament:filament+math+float3))

(warp-intricate-builder-option indirect-light :cubemap-irradiance
    %filament:filament+irradiance
  '(:pointer %filament:filament+indirect-light+builder)
  '(:pointer %filament:filament+texture))

(warp-intricate-builder-option indirect-light :intensity
    %filament:filament+intensity
  '(:pointer %filament:filament+indirect-light+builder)
  ':float)

(warp-intricate-builder-option indirect-light :rotation
    %filament:filament+rotation
  '(:pointer %filament:filament+indirect-light+builder)
  '(:pointer %filament:filament+math+mat3f))

(defmacro with-indirect-light-builder ((name &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:filament+indirect-light+builder))
         (build-expander (builder)
           `(%filament:filament+build
             '(:pointer %filament:filament+indirect-light+builder) ,builder
             '(:pointer %filament:filament+engine) !::engine)))
    (explode-builder name
                     'indirect-light
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-indirect-light (engine light)
  (%filament:filament+destroy
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::filament+indirect-light) light))
