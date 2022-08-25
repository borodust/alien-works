(cl:in-package :%alien-works.filament)

;;;
;;; VERTEX BUFFER
;;;
(u:define-enumval-extractor vertex-attribute-enum %filament:vertex-attribute)
(u:define-enumval-extractor vertex-attribute-type-enum %filament:vertex-buffer+attribute-type)

(warp-intricate-builder-option vertex-buffer-builder :buffer-count
    %filament:buffer-count
  '(claw-utils:claw-pointer %filament:vertex-buffer+builder)
  '%filament:uint8-t)


(warp-intricate-builder-option vertex-buffer-builder :vertex-count
    %filament:vertex-count
  '(claw-utils:claw-pointer %filament:vertex-buffer+builder)
  '%filament:uint32-t)


(warp-intricate-builder-option vertex-buffer-builder :attribute
    %filament:attribute
  '(claw-utils:claw-pointer %filament:vertex-buffer+builder)
  '%filament:vertex-attribute
  '%filament:uint8-t
  '%filament:vertex-buffer+attribute-type
  '%filament:uint32-t
  '%filament:uint8-t)


(warp-intricate-builder-option vertex-buffer-builder :normalized
    %filament:normalized
  '(claw-utils:claw-pointer %filament:vertex-buffer+builder)
  '%filament:vertex-attribute
  ':bool)


(defmacro with-vertex-buffer-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:vertex-buffer+builder))
         (build-expander (builder)
           `(%filament:build
             '(claw-utils:claw-pointer %filament:vertex-buffer+builder) ,builder
             '(claw-utils:claw-pointer %filament:engine) !::engine)))
    (explode-builder name
                     'vertex-buffer-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-vertex-buffer (engine buffer)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament:engine) engine
   '(claw-utils:claw-pointer %filament:vertex-buffer) buffer))


(cffi:defcallback buffer-descriptor-release :void ((buffer-ptr :pointer)
                                                   (buffer-size :size)
                                                   (user-data :pointer))
  (declare (ignore buffer-ptr buffer-size))
  (u:perform-foreign-callback user-data))


(defun update-vertex-buffer (buffer engine index data size
                             &optional (offset 0) done-callback)
  (iffi:with-intricate-instance
      (descriptor %filament:backend+buffer-descriptor
                  '(claw-utils:claw-pointer :void) data
                  '%filament:size-t size
                  '%filament:backend+buffer-descriptor+callback (cffi:null-pointer)
                  '(claw-utils:claw-pointer :void) (cffi:null-pointer))

    (when done-callback
      (u:register-foreign-callback descriptor done-callback)
      (%filament:backend+set-callback
       '(claw-utils:claw-pointer %filament::backend+buffer-descriptor) descriptor
       '%filament::backend+buffer-descriptor+callback (cffi:callback
                                                       buffer-descriptor-release)
       '(claw-utils:claw-pointer :void) descriptor))

    (%filament:set-buffer-at
     '(claw-utils:claw-pointer %filament:vertex-buffer) buffer
     '(claw-utils:claw-pointer %filament:engine) engine
     '%filament:uint8-t index
     '(claw-utils:claw-pointer %filament:vertex-buffer+buffer-descriptor) descriptor
     '%filament:uint32-t offset)))


;;;
;;; INDEX BUFFER
;;;
(u:define-enumval-extractor index-type-enum %filament:index-buffer+index-type)

(defun expand-index-buffer-builder-function (name)
  (ecase name
    (:index-count
     '(%filament:index-count
       '(claw-utils:claw-pointer %filament:index-buffer+builder)
       '%filament:uint32-t))
    (:buffer-type
     '(%filament:buffer-type
       '(claw-utils:claw-pointer %filament:index-buffer+builder)
       '%filament:index-buffer+index-type))))


(warp-intricate-builder-option index-buffer-builder :index-count
    %filament:index-count
  '(claw-utils:claw-pointer %filament:index-buffer+builder)
  '%filament:uint32-t)


(warp-intricate-builder-option index-buffer-builder :buffer-type
    %filament:buffer-type
  '(claw-utils:claw-pointer %filament:index-buffer+builder)
  '%filament:index-buffer+index-type)


(defmacro with-index-buffer-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:index-buffer+builder))
         (build-expander (builder)
           `(%filament:build
             '(claw-utils:claw-pointer %filament:index-buffer+builder) ,builder
             '(claw-utils:claw-pointer %filament:engine) !::engine)))
    (explode-builder name
                     'index-buffer-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-index-buffer (engine buffer)
  (%filament:destroy
   '(claw-utils:claw-pointer %filament:engine) engine
   '(claw-utils:claw-pointer %filament:index-buffer) buffer))


(defun update-index-buffer (buffer engine data size &optional (offset 0) done-callback)
  (iffi:with-intricate-instance
      (descriptor %filament:backend+buffer-descriptor
                  '(claw-utils:claw-pointer :void) data
                  '%filament:size-t size
                  '%filament:backend+buffer-descriptor+callback (cffi:null-pointer)
                  '(claw-utils:claw-pointer :void) (cffi:null-pointer))

    (when done-callback
      (u:register-foreign-callback descriptor done-callback)
      (%filament:backend+set-callback
       '(claw-utils:claw-pointer %filament::backend+buffer-descriptor) descriptor
       '%filament::backend+buffer-descriptor+callback (cffi:callback
                                                       buffer-descriptor-release)
       '(claw-utils:claw-pointer :void) descriptor))

    (%filament:set-buffer
     '(claw-utils:claw-pointer %filament:index-buffer) buffer
     '(claw-utils:claw-pointer %filament:engine) engine
     '(claw-utils:claw-pointer %filament:index-buffer+buffer-descriptor) descriptor
     '%filament:uint32-t offset)))
