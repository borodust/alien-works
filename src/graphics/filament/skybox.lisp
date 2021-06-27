(cl:in-package :%alien-works.filament)

;;;
;;; SKYBOX
;;;
(warp-intricate-builder-option skybox-builder :environment
    %filament:environment
  '(claw-utils:claw-pointer %filament:skybox+builder)
  '(claw-utils:claw-pointer %filament:texture))

(warp-intricate-builder-option skybox-builder :show-sun
    %filament:show-sun
  '(claw-utils:claw-pointer %filament:skybox+builder)
  ':bool)

(warp-intricate-builder-option skybox-builder :intensity
    %filament:intensity
  '(claw-utils:claw-pointer %filament:skybox+builder)
  ':float)

(warp-intricate-builder-option skybox-builder :color
    %filament:color
  '(claw-utils:claw-pointer %filament:skybox+builder)
  '(claw-utils:claw-pointer %filament:math+float4))


(defmacro with-skybox-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:skybox+builder))
         (build-expander (builder)
           `(%filament:build
             '(claw-utils:claw-pointer %filament:skybox+builder) ,builder
             '(claw-utils:claw-pointer %filament:engine) !::engine)))
    (explode-builder name
                     'skybox-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))
