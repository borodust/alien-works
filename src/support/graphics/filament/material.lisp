(cl:in-package :%alien-works.support.graphics)

;;;
;;; MATERIAL COMPILER
;;;
(defun parse-material (source &optional material-base-path)
  (let ((name (namestring (merge-pathnames
                           "in-memory"
                           (uiop:ensure-directory-pathname (or material-base-path ""))))))
    (iffi:with-intricate-instances ((config %filament.util:claw+filament+in-memory-config
                                            'claw-utils:claw-string name
                                            'claw-utils:claw-string source
                                            ;; FIXME: this is not really a good way
                                            ;; to figure out real unicode string length
                                            '%filament.util:size-t (length source))
                                    (compiler %filament.util:matc+material-compiler))
      (%filament.util:matc+run '(:pointer %filament.util::matc+material-compiler) compiler
                          '(:pointer %filament.util::matc+config) config)

      (let* ((out (%filament.util:claw+filament+get-output
                   :const
                   '(:pointer %filament.util::claw+filament+in-memory-config) config)))
        (%filament.util:claw+filament+material-data
         '(:pointer %filament.util::claw+filament+in-memory-output) out)))))


(defun destroy-material (data)
  (iffi:destroy-intricate-instance '%filament.util:claw+filament+material-data data))


(defun material-data (data)
  (%filament.util:claw+filament+data
   '(:pointer %filament.util::claw+filament+material-data) data))


(defun material-size (data)
  (%filament.util:claw+filament+size
   '(:pointer %filament.util::claw+filament+material-data) data))


(defmacro with-parsed-material ((material source &optional base-path) &body body)
  `(let ((,material (parse-material ,source ,base-path)))
     (unwind-protect
          (progn ,@body)
       (destroy-material ,material))))
