(cl:in-package :%alien-works.filament)


(defun create-view (engine)
  (%filament:filament+create-view
   '(:pointer %filament::filament+engine) engine))


(defun destroy-view (engine view)
  (%filament:filament+destroy
   '(:pointer %filament::filament+engine) engine
   '(:pointer %filament::filament+view) view))


(defun (setf view-camera) (camera view)
  (%filament::filament+set-camera
   '(:pointer %filament::filament+view) view
   '(:pointer %filament::filament+camera) camera))


(defun (setf view-scene) (scene view)
  (%filament::filament+set-scene
   '(:pointer %filament::filament+view) view
   '(:pointer %filament::filament+scene) scene))


(u:define-enumval-extractor view-anti-aliasing-enum %filament:filament+view+anti-aliasing)

(defun (setf view-anti-aliasing) (antialiasing view)
  (%filament::filament+set-anti-aliasing
   '(:pointer %filament::filament+view) view
   '%filament::filament+view+anti-aliasing (view-anti-aliasing-enum antialiasing)))


(defun (setf view-post-processing-enabled-p) (enabled-p view)
  (%filament::filament+set-post-processing-enabled
   '(:pointer %filament::filament+view) view
   :bool enabled-p))


(defun update-view-viewport (view x y width height)
  (let ((viewport (iffi:make-intricate-instance '%filament:filament+viewport
                                                '%filament::int32-t (coerce x 'fixnum)
                                                '%filament::int32-t (coerce y 'fixnum)
                                                '%filament::uint32-t (coerce width 'fixnum)
                                                '%filament::uint32-t (coerce height 'fixnum))))
    (unwind-protect
         (%filament::filament+set-viewport
          '(:pointer %filament::filament+view) view
          '(:pointer %filament::filament+viewport) viewport))
    (iffi:destroy-intricate-instance '%filament:filament+viewport viewport)))
