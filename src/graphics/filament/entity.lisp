(cl:in-package :%alien-works.graphics)


(defun create-entity ()
  (let ((mem (iffi:intricate-alloc '%filament:utils-entity)))
    (%filament:utils-create
     '(:pointer %filament:utils-entity) mem
     '(:pointer %filament:utils-entity-manager) (%filament:utils-entity-manager-get))))


(defun destroy-entity (entity)
  (%filament:utils-destroy
   '(:pointer %filament:utils-entity-manager) (%filament:utils-entity-manager-get)
   '(:pointer %filament:utils-entity) entity)
  (iffi:intricate-free entity))


(defun destroy-engine-entity (engine entity)
  (%filament:filament-destroy
   '(:pointer %filament:filament-engine) engine
   '(:pointer %filament:utils-entity) entity)
  (iffi:intricate-free entity))
