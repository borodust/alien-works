(cl:in-package :%alien-works.graphics)

;;;
;;; SKYBOX
;;;
(warp-intricate-builder-option skybox-builder :environment
    %filament:filament+environment
  '(:pointer %filament:filament+skybox+builder)
  '(:pointer %filament:filament+texture))

(warp-intricate-builder-option skybox-builder :show-sun
    %filament:filament+show-sun
  '(:pointer %filament:filament+skybox+builder)
  ':bool)

(warp-intricate-builder-option skybox-builder :intensity
    %filament:filament+intensity
  '(:pointer %filament:filament+skybox+builder)
  ':float)

(warp-intricate-builder-option skybox-builder :color
    %filament:filament+color
  '(:pointer %filament:filament+skybox+builder)
  '(:pointer %filament:filament+math+float4))


(defmacro with-skybox-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament+skybox+builder))
         (build-expander (builder)
           `(%filament:filament+build
             '(:pointer %filament:filament+skybox+builder) ,builder
             '(:pointer %filament:filament+engine) !::engine)))
    (explode-builder name
                     'skybox-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))
