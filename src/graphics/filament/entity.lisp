(cl:in-package :alien-works.graphics.filament)


(defun create-entity ()
  (let ((mem (iffi:intricate-alloc '%filament:utils-entity)))
    (%filament:utils-create
     '(:pointer %filament:utils-entity) mem
     '(:pointer %filament::utils-entity-manager) (%filament:utils-entity-manager-get))))


(defun destroy-entity (entity)
  (%filament:utils-destroy
   '(:pointer %filament::utils-entity-manager) (%filament:utils-entity-manager-get)
   '(:pointer %filament:utils-entity) entity)
  (iffi:intricate-free entity))
