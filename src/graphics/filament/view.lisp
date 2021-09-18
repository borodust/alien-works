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
(u:define-enumval-extractor view-dithering-enum %filament:view+dithering)


(defun (setf view-anti-aliasing) (antialiasing view)
  (%filament::set-anti-aliasing
   '(claw-utils:claw-pointer %filament::view) view
   '%filament::view+anti-aliasing (view-anti-aliasing-enum antialiasing)))


(defun (setf view-sample-count) (count view)
  (%filament::set-sample-count
   '(claw-utils:claw-pointer %filament::view) view
   '%filament::uint8-t (floor count)))


(defun (setf view-dithering) (dithering view)
  (%filament::set-dithering
   '(claw-utils:claw-pointer %filament::view) view
   '%filament::view+dithering (view-dithering-enum dithering)))


(defun update-view-bloom-options (view &key (enabled nil enabled-provided-p))
  (iffi:with-intricate-instance (bloom-options %filament:bloom-options)
    (iffi:with-intricate-slots %filament:bloom-options
        ((bloom-enabled %filament:enabled)) bloom-options
      (when enabled-provided-p
        (setf bloom-enabled enabled))
      (%filament::set-bloom-options
       '(claw-utils:claw-pointer %filament::view) view
       '(claw-utils:claw-pointer %filament::view+bloom-options) bloom-options))))


(defun disable-view-color-grading (view)
  (%filament::set-color-grading
   '(claw-utils:claw-pointer %filament::view) view
   '(claw-utils:claw-pointer %filament::color-grading) (cffi:null-pointer)))


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
