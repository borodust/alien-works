(cl:in-package :%alien-works.graphics)

(defun create-engine ()
  (%filament:filament-engine-create
   '%filament:filament-engine-backend (cffi:foreign-enum-value
                                       '%filament:filament-engine-backend
                                       :opengl)
   '(:pointer %filament:filament-engine-platform) (cffi:null-pointer)
   '(:pointer :void) (cffi:null-pointer)))


(defun destroy-engine (engine)
  (%filament:filament-engine-destroy
   '(:pointer %filament::filament-engine) engine))


(defun bundled-material-size (keyword)
  (let ((name (a:format-symbol '%filament "*~A~A~A*" 'materials- keyword '-size)))
    (eval name)))


(define-compiler-macro bundled-material-size (&whole whole keyword)
  (let ((name (a:format-symbol '%filament "*~A~A~A*" 'materials- keyword '-size)))
    (a:if-let ((size (handler-case
                         (eval name)
                       (t () nil))))
      size
      whole)))


(defun bundled-material-offset (keyword)
  (let ((name (a:format-symbol (find-package '%filament) "*~A~A~A*" 'materials- keyword '-offset)))
    (eval name)))


(define-compiler-macro bundled-material-offset (&whole whole keyword)
  (let ((name (a:format-symbol (find-package '%filament) "*~A~A~A*" 'materials- keyword '-offset)))
    (a:if-let ((offset (handler-case
                           (eval name)
                         (t () nil))))
      offset
      whole)))


(defun bundled-material-data (keyword)
  (a:when-let ((offset (bundled-material-offset keyword)))
    (cffi:make-pointer (+ (cffi:pointer-address %filament:*materials-package*) offset))))


(define-compiler-macro bundled-material-data (&whole whole keyword)
  (a:if-let ((offset (bundled-material-offset keyword)))
    `(cffi:make-pointer ,(+ (cffi:pointer-address %filament:*materials-package*) offset))
    whole))

;;;
;;; SKYBOX
;;;
(defun expand-skybox-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:environment
       (%explode-function
        '(%filament:filament-environment
          '(:pointer %filament:filament-skybox-builder)
          '(:pointer %filament:filament-texture))))
      (:show-sun
       (%explode-function
        '(%filament:filament-show-sun
          '(:pointer %filament:filament-skybox-builder)
          ':bool)))
      (:intensity
       (%explode-function
        '(%filament:filament-intensity
          '(:pointer %filament:filament-skybox-builder)
          ':float)))
      (:color
       (%explode-function
        '(%filament:filament-color
          '(:pointer %filament:filament-skybox-builder)
          '(:pointer %filament:filament-math-float4)))))))


(defmacro with-skybox-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament-skybox-builder))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-skybox-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine)))
    (explode-builder name
                     #'expand-skybox-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


;;;
;;; MATERIAL
;;;
(defun expand-material-builder-function (name args)
  (flet ((%explode-function (signature)
           (explode-function signature args)))
    (ecase name
      (:package
       (%explode-function
        '(%filament:filament-package
          '(:pointer %filament::filament-material-builder)
          '(:pointer :void)
          '%filament:size-t))))))


(defmacro with-material-builder ((name &rest steps) &body body)
  (flet ((ctor-expander ()
           '(%filament:filament-material-builder))
         (build-expander (builder)
           `(%filament:filament-build
             '(:pointer %filament:filament-material-builder) ,builder
             '(:pointer %filament:filament-engine) !::engine)))
    (explode-builder name
                     #'expand-material-builder-function
                     #'ctor-expander
                     #'build-expander
                     '(!::engine)
                     steps
                     body)))


(defun material-default-instance (material)
  (%filament:filament-get-default-instance
   '(:pointer %filament::filament-material) material))
