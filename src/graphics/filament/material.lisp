(cl:in-package :%alien-works.graphics)


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
;;; MATERIAL PARSER
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


;;;
;;; MATERIAL COMPILER
;;;
(defun parse-material (source &optional material-base-path)
  (let ((name (namestring (merge-pathnames
                           "in-memory"
                           (uiop:ensure-directory-pathname (or material-base-path ""))))))
    (iffi:with-intricate-instances ((config %filament:claw-filament-in-memory-config
                                            'claw-utils:claw-string name
                                            'claw-utils:claw-string source
                                            ;; FIXME: this is not really a good way
                                            ;; to figure out real unicode string length
                                            '%filament:size-t (length source))
                                    (compiler %filament:matc-material-compiler))
      (%filament:matc-run '(:pointer %filament::matc-material-compiler) compiler
                          '(:pointer %filament::matc-config) config)

      (let* ((out (%filament:claw-filament-get-output
                   '(:pointer %filament::claw-filament-in-memory-config) config)))
        (%filament:claw-filament-material-data
         '(:pointer %filament::claw-filament-in-memory-output) out)))))


(defun destroy-material (data)
  (iffi:destroy-intricate-instance '%filament:claw-filament-material-data data))


(defun material-data (data)
  (%filament:claw-filament-data
   '(:pointer %filament::claw-filament-material-data) data))


(defun material-size (data)
  (%filament:claw-filament-size
   '(:pointer %filament::claw-filament-material-data) data))


(defmacro with-parsed-material ((material source) &body body)
  `(let ((,material (parse-material ,source)))
     (unwind-protect
          (progn ,@body)
       (destroy-material ,material))))
