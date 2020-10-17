(cl:defpackage :alien-works
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria))
  (:use :cl))
(cl:in-package :alien-works)

(defparameter *rendering-backend* :opengl)

;;;
;;; UTIL
;;;
(defun load-blobs ()
  (list
   (cffi:load-foreign-library
    "libSDL2.so")
   (cffi:load-foreign-library
    "/home/borodust/devel/repo/claw-filament/src/lib/build/libfilament.clawed.so")))


;;;
;;; SDL
;;;
(defun call-with-window (callback)
  (%sdl:init %sdl:+init-video+)
  (let ((window (cffi:with-foreign-string (name "YO")
                  (%sdl:create-window name
                                      %sdl:+windowpos-undefined+
                                      %sdl:+windowpos-undefined+
                                      640 480
                                      (cffi:foreign-enum-value '%sdl:window-flags
                                                               *rendering-backend*)))))
    (when (cffi:null-pointer-p window)
      (error "Failed to create a window"))
    (unwind-protect
         (funcall callback window)
      (%sdl:destroy-window window)
      (%sdl:quit))))


(defmacro with-window ((window) &body body)
  `(call-with-window (lambda (,window) ,@body)))


(defun native-window (window)
  (cref:c-with ((wm-info %sdl:sys-w-minfo))
    (setf (wm-info :version :major) %sdl:+major-version+
          (wm-info :version :minor) %sdl:+minor-version+
          (wm-info :version :patch) %sdl:+patchlevel+)

    (%sdl:get-window-wm-info window (wm-info &))

    (ecase (wm-info :subsystem)
      (:x11 (cffi:make-pointer (wm-info :info :x11 :window))))))

;;;
;;; FILAMENT
;;;
(defun %vec4f (vec idx)
  (cffi:mem-ref (%filament::filament-math-details-operator[]
                 '(:pointer %filament::filament-math-details-t-vec4<float>) vec
                 '%filament::size-t idx)
                :float))


(defun (setf %vec4f) (value vec idx)
  (setf (cffi:mem-ref (%filament::filament-math-details-operator[]
                       '(:pointer %filament::filament-math-details-t-vec4<float>) vec
                       '%filament::size-t idx)
                      :float)
        (float value 0f0)))


(defun %create-vec4f (x y z w)
  (let ((vec (iffi:make-simple-intricate-instance '%filament:filament-math-details-t-vec4<float>)))
    (setf (%vec4f vec 0) x
          (%vec4f vec 1) y
          (%vec4f vec 2) z
          (%vec4f vec 3) w)
    vec))

(defun %create-engine ()
  (%filament:filament-create
   '%filament:filament-engine-backend (cffi:foreign-enum-value
                                       '%filament:filament-engine-backend
                                       *rendering-backend*)
   '(:pointer %filament:filament-engine-platform) (cffi:null-pointer)
   '(:pointer :void) (cffi:null-pointer)))


(defun %destroy-engine (engine)
  (%filament:filament-destroy
   '(:pointer %filament::filament-engine) engine))


(defun %create-renderer (engine)
  (%filament:filament-create-renderer '(:pointer %filament::filament-engine) engine))


(defun %render-view (renderer view)
  (%filament::filament-render
   '(:pointer %filament::filament-renderer) renderer
   '(:pointer %filament::filament-view) view))


(defun %destroy-renderer (engine renderer)
  (%filament:filament-destroy
   '(:pointer %filament::filament-engine) engine
   '(:pointer %filament::filament-renderer) renderer))


(defun %create-swap-chain (engine native-window)
  (%filament::filament-create-swap-chain
   '(:pointer %filament::filament-engine) engine
   '(:pointer :void) native-window
   '%filament::uint64-t 0))


(defun %destroy-swap-chain (engine swap-chain)
  (%filament:filament-destroy
   '(:pointer %filament::filament-engine) engine
   '(:pointer %filament::filament-swap-chain) swap-chain))


(defun %create-scene (engine)
  (%filament::filament-create-scene
   '(:pointer %filament::filament-engine) engine))


(defun %destroy-scene (engine scene)
  (%filament:filament-destroy
   '(:pointer %filament::filament-engine) engine
   '(:pointer %filament::filament-scene) scene))


(defun %create-camera (engine)
  (%filament:filament-create-camera
   '(:pointer %filament::filament-engine) engine))


(defun %destroy-camera (engine camera)
  (%filament:filament-destroy
   '(:pointer %filament::filament-engine) engine
   '(:pointer %filament::filament-camera) camera))


(defun %create-view (engine)
  (%filament:filament-create-view
   '(:pointer %filament::filament-engine) engine))


(defun %destroy-view (engine view)
  (%filament:filament-destroy
   '(:pointer %filament::filament-engine) engine
   '(:pointer %filament::filament-view) view))


(defun %create-color-skybox (engine r g b a)
  (let ((builder (iffi:make-intricate-instance '%filament::filament-skybox-builder
                                               '%filament::filament-builder)))
    (%filament:filament-skybox-color
     '(:pointer %filament::filament-skybox-builder) builder
     '(:pointer %filament::filament-math-float4) (%create-vec4f r g b a))

    (prog1 (%filament:filament-skybox-build
            '(:pointer %filament::filament-skybox-builder) builder
            '(:pointer %filament::filament-engine) engine)
      (iffi:destroy-intricate-instance '%filament::filament-skybox-builder
                                       '%filament::filament-skybox-~builder
                                       builder))))

(defclass engine ()
  (engine swap-chain renderer scene camera view))


(defmethod initialize-instance :after ((this engine) &key native-window)
  (with-slots (engine swap-chain renderer scene camera view) this
    (setf engine (%create-engine)
          swap-chain (%create-swap-chain engine native-window)
          renderer (%create-renderer engine)
          camera (%create-camera engine)
          view (%create-view engine)
          scene (%create-scene engine))
    ;; scene setup
    (%filament::filament-set-skybox
     '(:pointer %filament::filament-scene) scene
     '(:pointer %filament::filament-skybox) (%create-color-skybox engine 0.8 0.25 0.25 1))

    ;; view setup
    (%filament::filament-set-camera
     '(:pointer %filament::filament-view) view
     '(:pointer %filament::filament-camera) camera)
    (%filament::filament-set-scene
     '(:pointer %filament::filament-view) view
     '(:pointer %filament::filament-scene) scene)
    (%filament::filament-set-post-processing-enabled
     '(:pointer %filament::filament-view) view
     :bool nil)

    (let ((viewport (iffi:make-simple-intricate-instance '%filament:filament-viewport
                                                         '%filament::int32-t 0
                                                         '%filament::int32-t 0
                                                         '%filament::uint32-t 640
                                                         '%filament::uint32-t 480)))
      (%filament::filament-set-viewport
       '(:pointer %filament::filament-view) view
       '(:pointer %filament::filament-viewport) viewport)
      (iffi:destroy-intricate-instance '%filament:filament-viewport
                                       '%filament:filament-~viewport
                                       viewport))))


(defun create-engine (native-window)
  (make-instance 'engine :native-window native-window))


(defun destroy-engine (engine)
  (with-slots (engine swap-chain renderer scene camera view) engine
    (%destroy-view engine view)
    (%destroy-camera engine camera)
    (%destroy-scene engine scene)
    (%destroy-renderer engine renderer)
    (%destroy-swap-chain engine swap-chain)
    (%destroy-engine engine)))


(defun begin-frame (engine)
  (with-slots (engine renderer swap-chain) engine
    (%filament:filament-begin-frame
     '(:pointer %filament::filament-renderer) renderer
     '(:pointer %filament::filament-swap-chain) swap-chain
     '%filament::uint64-t 0
     '%filament::filament-backend-frame-finished-callback (cffi:null-pointer)
     '(:pointer :void) (cffi:null-pointer))))


(defun end-frame (engine)
  (with-slots (renderer) engine
    (%filament:filament-end-frame
     '(:pointer %filament::filament-renderer) renderer)))


(defmacro with-engine ((engine &key native-window) &body body)
  `(let ((,engine (create-engine ,native-window)))
     (unwind-protect
          (progn ,@body)
       (destroy-engine ,engine))))


(defun render-frame (engine)
  (with-slots (renderer view) engine
    (when (begin-frame engine)
      (%render-view renderer view)
      (end-frame engine))))

;;;
;;; DEMO
;;;
(defun run ()
  (with-window (win)
    (with-engine (engine :native-window (native-window win))
      (loop repeat 5
            do (render-frame engine)
               (sleep 1)))))
