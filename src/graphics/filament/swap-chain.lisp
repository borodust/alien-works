(cl:in-package :%alien-works.graphics)


(defun create-swap-chain (engine native-window)
  (%filament::filament+create-swap-chain
   '(:pointer %filament::filament+engine) engine
   '(:pointer :void) native-window
   '%filament::uint64-t 0))


(defun destroy-swap-chain (engine swap-chain)
  (%filament:filament+destroy
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::filament+swap-chain) swap-chain))
