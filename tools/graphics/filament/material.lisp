(cl:in-package :%alien-works.tools.filament)


(u:define-enumval-extractor target-api-enum %filament.util:matc+config+target-api)
(u:define-enumval-extractor platform-enum %filament.util:matc+config+platform)
(u:define-enumval-extractor optimization-enum %filament.util:matc+config+optimization)

;;;
;;; MATERIAL COMPILER
;;;
(defun parse-material (source &key base-path debug
                                target-api
                                platform
                                optimization)
  ;; fixme: pray i can convince filament authors to remove assert for material
  ;; file existence
  (uiop:with-temporary-file (:pathname temp-file :directory base-path)
    (let ((name (uiop:native-namestring temp-file)))
      (iffi:with-intricate-instances ((config %filament.util:claw+filament+in-memory-config
                                              'claw-utils:claw-string name
                                              'claw-utils:claw-string source
                                              ;; FIXME: this is not really a good way
                                              ;; to figure out real unicode string length
                                              '%filament.util:size-t (length source))
                                      (compiler %filament.util:matc+material-compiler))

        (%filament.util:claw+filament+set-debug
         '(claw-utils:claw-pointer %filament.util::claw+filament+in-memory-config) config
         :bool (and debug t))

        (%filament.util:claw+filament+set-target-api
         '(claw-utils:claw-pointer %filament.util::claw+filament+in-memory-config) config
         '%filament.util::matc+config+target-api (or target-api :all))

        (%filament.util:claw+filament+set-platform
         '(claw-utils:claw-pointer %filament.util::claw+filament+in-memory-config) config
         '%filament.util::matc+config+platform (or platform :all))

        (%filament.util:claw+filament+set-output-format
         '(claw-utils:claw-pointer %filament.util::claw+filament+in-memory-config) config
         '%filament.util::matc+config+output-format :blob)

        (%filament.util:matc+set-optimization-level
         '(claw-utils:claw-pointer %filament.util::matc+config) config
         '%filament.util::matc+config+optimization (or optimization :performance))

        (%filament.util:matc+run '(:pointer %filament.util::matc+material-compiler) compiler
                                 '(:pointer %filament.util::matc+config) config)

        (let* ((out (%filament.util:claw+filament+get-output
                     :const
                     '(:pointer %filament.util::claw+filament+in-memory-config) config)))
          (%filament.util:claw+filament+material-data
           '(:pointer %filament.util::claw+filament+in-memory-output) out))))))


(defun destroy-material (data)
  ;; we can't use intricate destroy here because instance was allocated in
  ;; foreign code using unaligend (default) allocation
  (%filament.util:claw+filament+~material-data
   '(:pointer %filament.util:claw+filament+material-data) data))


(defun material-data (data)
  (%filament.util:claw+filament+data
   '(:pointer %filament.util::claw+filament+material-data) data))


(defun material-size (data)
  (%filament.util:claw+filament+size
   '(:pointer %filament.util::claw+filament+material-data) data))


(defmacro with-parsed-material ((material source &key base-path debug
                                                   target-api
                                                   platform
                                                   optimization)
                                &body body)
  `(let ((,material (parse-material ,source
                                    ,@(when base-path
                                        `(:base-path ,base-path))
                                    ,@(when debug
                                        `(:debug ,debug))
                                    ,@(when target-api
                                        `(:target-api ,target-api))
                                    ,@(when platform
                                        `(:platform ,platform))
                                    ,@(when optimization
                                        `(:optimization ,optimization)))))
     (unwind-protect
          (progn ,@body)
       (destroy-material ,material))))
