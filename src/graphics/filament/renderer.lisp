(cl:in-package :%alien-works.filament)


(defun create-renderer (engine)
  (%filament:create-renderer '(claw-utils:claw-pointer %filament::engine) engine))


(defun destroy-renderer (engine renderer)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::renderer) renderer))


(defun update-renderer-clear-options (renderer &key clear-color
                                                 (clear nil clear-provided-p)
                                                 (discard nil discard-provided-p))
  (iffi:with-intricate-instance (opts %filament::renderer+clear-options)
    (iffi:with-intricate-slots %filament::renderer+clear-options
        ((%clear-color %filament:clear-color)
         (%clear %filament:clear)
         (%discard %filament:discard))
        opts
      (when clear-color
        (with-vec4f (new-clear-color
                     (m:vec4 clear-color 0)
                     (m:vec4 clear-color 1)
                     (m:vec4 clear-color 2)
                     (m:vec4 clear-color 3))
          (setf %clear-color new-clear-color)))
      (when clear-provided-p
        (setf %clear (and clear t)))
      (when discard-provided-p
        (setf %discard (and discard t))))
    (%filament::set-clear-options
     '(claw-utils:claw-pointer %filament::renderer) renderer
     '(claw-utils:claw-pointer %filament::renderer+clear-options) opts)))


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
