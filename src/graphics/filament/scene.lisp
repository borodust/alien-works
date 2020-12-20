(cl:in-package :%alien-works.graphics)


(defun create-scene (engine)
  (%filament::filament+create-scene
   '(:pointer %filament::filament+engine) engine))


(defun destroy-scene (engine scene)
  (%filament:filament+destroy
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::filament+scene) scene))


(defun (setf scene-skybox) (skybox scene)
  (%filament::filament+set-skybox
   '(:pointer %filament::filament+scene) scene
   '(:pointer %filament::filament+skybox) skybox)
  skybox)


(defun add-scene-entity (scene entity)
  (%filament::filament+add-entity
   '(:pointer %filament::filament+scene) scene
   '(:pointer %filament::utils+entity) entity))
