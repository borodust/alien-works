(cl:in-package :%alien-works.filament)

;;;
;;; VERTEX BUFFER
;;;
(u:define-enumval-extractor vertex-attribute-enum %filament:filament+vertex-attribute)
(u:define-enumval-extractor vertex-attribute-type-enum %filament:filament+vertex-buffer+attribute-type)

(warp-intricate-builder-option vertex-buffer-builder :buffer-count
    %filament:filament+buffer-count
  '(:pointer %filament:filament+vertex-buffer+builder)
  '%filament:uint8-t)


(warp-intricate-builder-option vertex-buffer-builder :vertex-count
    %filament:filament+vertex-count
  '(:pointer %filament:filament+vertex-buffer+builder)
  '%filament:uint32-t)


(warp-intricate-builder-option vertex-buffer-builder :attribute
    %filament:filament+attribute
  '(:pointer %filament:filament+vertex-buffer+builder)
  '%filament:filament+vertex-attribute
  '%filament:uint8-t
  '%filament:filament+vertex-buffer+attribute-type
  '%filament:uint32-t
  '%filament:uint8-t)


(warp-intricate-builder-option vertex-buffer-builder :normalized
    %filament:filament+normalized
  '(:pointer %filament:filament+vertex-buffer+builder)
  '%filament:filament+vertex-attribute
  ':bool)


(defmacro with-vertex-buffer-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament+vertex-buffer+builder))
         (build-expander (builder)
           `(%filament:filament+build
             '(:pointer %filament:filament+vertex-buffer+builder) ,builder
             '(:pointer %filament:filament+engine) !::engine)))
    (explode-builder name
                     'vertex-buffer-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-vertex-buffer (engine buffer)
  (%filament:filament+destroy
   '(:pointer %filament:filament+engine) engine
   '(:pointer %filament:filament+vertex-buffer) buffer))


(defun update-vertex-buffer (buffer engine index data size &optional (offset 0))
  (iffi:with-intricate-instance
      (descriptor %filament:filament+backend+buffer-descriptor
                  '(:pointer :void) data
                  '%filament:size-t size
                  '%filament:filament+backend+buffer-descriptor+callback (cffi:null-pointer)
                  '(:pointer :void) (cffi:null-pointer))
    (%filament:filament+set-buffer-at
     '(:pointer %filament:filament+vertex-buffer) buffer
     '(:pointer %filament:filament+engine) engine
     '%filament:uint8-t index
     '(:pointer %filament:filament+vertex-buffer+buffer-descriptor) descriptor
     '%filament:uint32-t offset)))


;;;
;;; INDEX BUFFER
;;;
(u:define-enumval-extractor index-type-enum %filament:filament+index-buffer+index-type)

(defun expand-index-buffer-builder-function (name)
  (ecase name
    (:index-count
     '(%filament:filament+index-count
       '(:pointer %filament:filament+index-buffer+builder)
       '%filament:uint32-t))
    (:buffer-type
     '(%filament:filament+buffer-type
       '(:pointer %filament:filament+index-buffer+builder)
       '%filament:filament+index-buffer+index-type))))


(warp-intricate-builder-option index-buffer-builder :index-count
    %filament:filament+index-count
  '(:pointer %filament:filament+index-buffer+builder)
  '%filament:uint32-t)


(warp-intricate-builder-option index-buffer-builder :buffer-type
    %filament:filament+buffer-type
  '(:pointer %filament:filament+index-buffer+builder)
  '%filament:filament+index-buffer+index-type)


(defmacro with-index-buffer-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament+index-buffer+builder))
         (build-expander (builder)
           `(%filament:filament+build
             '(:pointer %filament:filament+index-buffer+builder) ,builder
             '(:pointer %filament:filament+engine) !::engine)))
    (explode-builder name
                     'index-buffer-builder
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun destroy-index-buffer (engine buffer)
  (%filament:filament+destroy
   '(:pointer %filament:filament+engine) engine
   '(:pointer %filament:filament+index-buffer) buffer))


(defun update-index-buffer (buffer engine data size &optional (offset 0))
  (iffi:with-intricate-instance
      (descriptor %filament:filament+backend+buffer-descriptor
                  '(:pointer :void) data
                  '%filament:size-t size
                  '%filament:filament+backend+buffer-descriptor+callback (cffi:null-pointer)
                  '(:pointer :void) (cffi:null-pointer))
    (%filament:filament+set-buffer
     '(:pointer %filament:filament+index-buffer) buffer
     '(:pointer %filament:filament+engine) engine
     '(:pointer %filament:filament+index-buffer+buffer-descriptor) descriptor
     '%filament:uint32-t offset)))
