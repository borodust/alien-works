(cl:in-package :%alien-works.filament)


(defun create-view (engine)
  (%filament:create-view
   '(claw-utils:claw-pointer %filament::engine) engine))


(defun destroy-view (engine view)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament::engine) engine
   '(claw-utils:claw-pointer %filament::view) view))


(defun (setf view-camera) (camera view)
  (%filament::set-camera
   '(claw-utils:claw-pointer %filament::view) view
   '(claw-utils:claw-pointer %filament::camera) camera))


(defun (setf view-scene) (scene view)
  (%filament::set-scene
   '(claw-utils:claw-pointer %filament::view) view
   '(claw-utils:claw-pointer %filament::scene) scene))


(u:define-enumval-extractor view-anti-aliasing-enum %filament:view+anti-aliasing)

(defun (setf view-anti-aliasing) (antialiasing view)
  (%filament::set-anti-aliasing
   '(claw-utils:claw-pointer %filament::view) view
   '%filament::view+anti-aliasing (view-anti-aliasing-enum antialiasing)))


(defun (setf view-post-processing-enabled-p) (enabled-p view)
  (%filament::set-post-processing-enabled
   '(claw-utils:claw-pointer %filament::view) view
   :bool enabled-p))


(defun update-view-viewport (view x y width height)
  (let ((viewport (iffi:make-intricate-instance '%filament:viewport
                                                '%filament::int32-t (coerce x 'fixnum)
                                                '%filament::int32-t (coerce y 'fixnum)
                                                '%filament::uint32-t (coerce width 'fixnum)
                                                '%filament::uint32-t (coerce height 'fixnum))))
    (unwind-protect
         (%filament::set-viewport
          '(claw-utils:claw-pointer %filament::view) view
          '(claw-utils:claw-pointer %filament::viewport) viewport))
    (iffi:destroy-intricate-instance '%filament:viewport viewport)))
