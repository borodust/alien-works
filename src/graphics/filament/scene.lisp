(cl:in-package :%alien-works.filament)


(defun create-scene (engine)
  (%filament::create-scene
   '(claw-utils:claw-pointer %filament::engine) engine))


(defun destroy-scene (engine scene)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::scene) scene))


(defun scene-skybox (scene)
  (%filament::get-skybox
   :const
   '(claw-utils:claw-pointer %filament::scene) scene))


(defun (setf scene-skybox) (skybox scene)
  (%filament::set-skybox
   '(claw-utils:claw-pointer %filament::scene) scene
   '(claw-utils:claw-pointer %filament::skybox) skybox)
  skybox)


(defun (setf scene-indirect-light) (indirect-light scene)
  (%filament::set-indirect-light
   '(claw-utils:claw-pointer %filament::scene) scene
   '(claw-utils:claw-pointer %filament::indirect-light) indirect-light)
  indirect-light)


(defun add-scene-entity (scene entity)
  (%filament::add-entity
   '(claw-utils:claw-pointer %filament::scene) scene
   '(claw-utils:claw-pointer %filament::utils+entity) entity))


(defun remove-scene-entity (scene entity)
  (%filament::remove
   '(claw-utils:claw-pointer %filament::scene) scene
   '(claw-utils:claw-pointer %filament::utils+entity) entity))
