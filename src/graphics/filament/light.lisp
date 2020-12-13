(cl:in-package :%alien-works.graphics)


(u:define-enumval-extractor light-type-enum %filament:filament-light-manager-type)


(defun expand-light-manager-builder-function (name)
  (ecase name
    (:cast-shadows
     '(%filament::filament-cast-shadows
       '(:pointer %filament::filament-light-manager-builder)
       ':bool))
    (:shadow-options
     '(%filament::filament-shadow-options
       '(:pointer %filament::filament-light-manager-builder)
       '(:pointer %filament::filament-light-manager-shadow-options)))
    (:cast-light
     '(%filament::filament-cast-light
       '(:pointer %filament::filament-light-manager-builder)
       ':bool))
    (:position
     '(%filament::filament-position
       '(:pointer %filament::filament-light-manager-builder)
       '(:pointer %filament::filament-math-float3)))
    (:direction
     '(%filament::filament-direction
       '(:pointer %filament::filament-light-manager-builder)
       '(:pointer %filament::filament-math-float3)))
    (:color
     '(%filament::filament-color
       '(:pointer %filament::filament-light-manager-builder)
       '(:pointer %filament::filament-linear-color)))
    (:intensity
     '(%filament::filament-intensity-candela
       '(:pointer %filament::filament-light-manager-builder)
       ':float))
    (:intensity-efficiency
     '(%filament::filament-intensity
       '(:pointer %filament::filament-light-manager-builder)
       ':float
       ':float))
    (:falloff
     '(%filament:filament-falloff
       '(:pointer %filament::filament-light-manager-builder)
       ':float))
    (:spot-light-cone
     '(%filament:filament-spot-light-cone
       '(:pointer %filament::filament-light-manager-builder)
       ':float
       ':float))
    (:sun-angular-radius
     '(%filament:filament-sun-angular-radius
       '(:pointer %filament::filament-light-manager-builder)
       ':float))
    (:sun-halo-size
     '(%filament:filament-sun-halo-size
       '(:pointer %filament::filament-light-manager-builder)
       ':float))
    (:sun-halo-falloff
     '(%filament:filament-sun-halo-falloff
       '(:pointer %filament::filament-light-manager-builder)
       ':float))))

(warp-intricate-function light-builder-cast-shadows
    %filament::filament-cast-shadows
  '(:pointer %filament::filament-light-manager-builder)
  ':bool)

(warp-intricate-function light-builder-shadow-options
    %filament::filament-shadow-options
  '(:pointer %filament::filament-light-manager-builder)
  '(:pointer %filament::filament-light-manager-shadow-options))

(warp-intricate-function light-builder-cast-light
    %filament::filament-cast-light
  '(:pointer %filament::filament-light-manager-builder)
  ':bool)

(warp-intricate-function light-builder-position
    %filament::filament-position
  '(:pointer %filament::filament-light-manager-builder)
  '(:pointer %filament::filament-math-float3))

(warp-intricate-function light-builder-direction
    %filament::filament-direction
  '(:pointer %filament::filament-light-manager-builder)
  '(:pointer %filament::filament-math-float3))

(warp-intricate-function light-builder-color
    %filament::filament-color
  '(:pointer %filament::filament-light-manager-builder)
  '(:pointer %filament::filament-linear-color))

(warp-intricate-function light-builder-intensity
    %filament::filament-intensity-candela
  '(:pointer %filament::filament-light-manager-builder)
  ':float)

(warp-intricate-function light-builder-intensity-efficiency
    %filament::filament-intensity
  '(:pointer %filament::filament-light-manager-builder)
  ':float
  ':float)

(warp-intricate-function light-builder-falloff
    %filament:filament-falloff
  '(:pointer %filament::filament-light-manager-builder)
  ':float)

(warp-intricate-function light-builder-spot-light-cone
    %filament:filament-spot-light-cone
  '(:pointer %filament::filament-light-manager-builder)
  ':float
  ':float)

(warp-intricate-function light-builder-sun-angular-radius
    %filament:filament-sun-angular-radius
  '(:pointer %filament::filament-light-manager-builder)
  ':float)

(warp-intricate-function light-builder-sun-halo-size
    %filament:filament-sun-halo-size
  '(:pointer %filament::filament-light-manager-builder)
  ':float)

(warp-intricate-function light-builder-sun-halo-falloff
    %filament:filament-sun-halo-falloff
  '(:pointer %filament::filament-light-manager-builder)
  ':float)


(defmacro with-light-builder ((name (type) &body steps) &body body)
  (flet ((ctor-expander ()
           `(%filament:filament-light-manager-builder '%filament:filament-light-manager-type
                                                      (light-type-enum ,type)))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-light-manager-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine
             '(:pointer %filament:utils-entity) !::entity)))
    (explode-builder name
                     #'expand-light-manager-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine !::entity)
                     steps
                     body)))
