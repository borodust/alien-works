(cl:in-package :%alien-works.graphics)


(defun create-renderer (engine)
  (%filament:filament+create-renderer '(:pointer %filament::filament+engine) engine))


(defun destroy-renderer (engine renderer)
  (%filament:filament+destroy
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::filament+renderer) renderer))


(defun render-view (renderer view)
  (%filament::filament+render
   '(:pointer %filament::filament+renderer) renderer
   '(:pointer %filament::filament+view) view))


(defun begin-frame (renderer swap-chain)
  (%filament:filament+begin-frame
   '(:pointer %filament::filament+renderer) renderer
   '(:pointer %filament::filament+swap-chain) swap-chain
   '%filament::uint64-t 0
   '%filament::filament+backend+frame-finished-callback (cffi:null-pointer)
   '(:pointer :void) (cffi:null-pointer)))


(defun end-frame (renderer)
  (%filament:filament+end-frame
   '(:pointer %filament::filament+renderer) renderer))
