(cl:in-package :%alien-works.filament)


(defun create-entity ()
  (let ((mem (iffi:intricate-alloc '%filament:utils+entity)))
    (%filament:utils+create
     '(claw-utils:claw-pointer %filament:utils+entity) mem
     '(claw-utils:claw-pointer %filament:utils+entity-manager) (%filament:utils+entity-manager+get))))


(defun destroy-entity (entity)
  (%filament:utils+destroy
   '(claw-utils:claw-pointer %filament:utils+entity-manager) (%filament:utils+entity-manager+get)
   '(claw-utils:claw-pointer %filament:utils+entity) entity)
  (iffi:intricate-free entity))


(defun destroy-engine-entity (engine entity)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament:engine) engine
   '(claw-utils:claw-pointer %filament:utils+entity) entity)
  (iffi:intricate-free entity))
