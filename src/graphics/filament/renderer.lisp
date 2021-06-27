(cl:in-package :%alien-works.filament)


(defun create-renderer (engine)
  (%filament:create-renderer '(claw-utils:claw-pointer %filament::engine) engine))


(defun destroy-renderer (engine renderer)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::renderer) renderer))


(defun render-view (renderer view)
  (%filament::render
   '(claw-utils:claw-pointer %filament::renderer) renderer
   '(claw-utils:claw-pointer %filament::view) view))


(defun begin-frame (renderer swap-chain)
  (%filament:begin-frame
   '(claw-utils:claw-pointer %filament::renderer) renderer
   '(claw-utils:claw-pointer %filament::swap-chain) swap-chain
   '%filament::uint64-t 0))


(defun end-frame (renderer)
  (%filament:end-frame
   '(claw-utils:claw-pointer %filament::renderer) renderer))
