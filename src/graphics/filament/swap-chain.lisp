(cl:in-package :%alien-works.filament)


(defun create-swap-chain (engine native-window)
  (%filament::create-swap-chain
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer :void) native-window
   '%filament::uint64-t 0))


(defun create-headless-swap-chain (engine width height)
  (%filament::create-swap-chain
   '(claw-utils:claw-pointer %filament::engine) engine
   '%filament::uint32-t (floor width)
   '%filament::uint32-t (floor height)
   '%filament::uint64-t 0))


(defun destroy-swap-chain (engine swap-chain)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::swap-chain) swap-chain))
