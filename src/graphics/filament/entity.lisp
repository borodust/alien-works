(cl:in-package :%alien-works.filament)


(defun entity-manager (engine)
  (%filament:get-entity-manager
     '(claw-utils:claw-pointer %filament::engine) engine))


(defun create-entity (entity-manager)
  (let ((entity (iffi:intricate-alloc '%filament:utils+entity)))
    (%filament:utils+create
     '(claw-utils:claw-pointer %filament:utils+entity) entity
     '(claw-utils:claw-pointer %filament:utils+entity-manager) entity-manager)))


(defun destroy-entity (entity-manager entity)
  (%filament:utils+destroy
   '(claw-utils:claw-pointer %filament:utils+entity-manager) entity-manager
   '(claw-utils:claw-pointer %filament:utils+entity) entity)
  (iffi:intricate-free entity))


(defun destroy-engine-entity (engine entity)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament:engine) engine
   '(claw-utils:claw-pointer %filament:utils+entity) entity))
