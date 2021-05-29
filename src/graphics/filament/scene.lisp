(cl:in-package :%alien-works.filament)


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


(defun (setf scene-indirect-light) (indirect-light scene)
  (%filament::filament+set-indirect-light
   '(:pointer %filament::filament+scene) scene
   '(:pointer %filament::filament+indirect-light) indirect-light)
  indirect-light)


(defun add-scene-entity (scene entity)
  (%filament::filament+add-entity
   '(:pointer %filament::filament+scene) scene
   '(:pointer %filament::utils+entity) entity))
