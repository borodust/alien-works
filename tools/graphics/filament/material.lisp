(cl:in-package :%alien-works.tools.filament)


(u:define-enumval-extractor target-api-enum %filament.util:matc+config+target-api)
(u:define-enumval-extractor platform-enum %filament.util:matc+config+platform)
(u:define-enumval-extractor optimization-enum %filament.util:matc+config+optimization)

;;;
;;; MATERIAL COMPILER
;;;
(defun serialize-material (source &key base-path debug
                                    target-api
                                    platform
                                    optimization)
  ;; fixme: pray i can convince filament authors to remove assert for material
  ;; file existence
  (uiop:with-temporary-file (:pathname temp-file :directory base-path)
    (let ((name (uiop:native-namestring temp-file)))
      (iffi:with-intricate-instances ((config %filament.util:aw+filament+tools+memio+in-memory-config
                                              'claw-utils:claw-string name
                                              'claw-utils:claw-string source
                                              ;; FIXME: this is not really a good way
                                              ;; to figure out real unicode string length
                                              '%filament.util:size-t (length source))
                                      (compiler %filament.util:matc+material-compiler))

        (%filament.util:aw+filament+tools+memio+set-debug
         '(claw-utils:claw-pointer %filament.util::aw+filament+tools+memio+in-memory-config) config
         :bool (and debug t))

        (%filament.util:aw+filament+tools+memio+set-target-api
         '(claw-utils:claw-pointer %filament.util::aw+filament+tools+memio+in-memory-config) config
         '%filament.util::matc+config+target-api (or target-api :all))

        (%filament.util:aw+filament+tools+memio+set-platform
         '(claw-utils:claw-pointer %filament.util::aw+filament+tools+memio+in-memory-config) config
         '%filament.util::matc+config+platform (or platform :all))

        (%filament.util:aw+filament+tools+memio+set-output-format
         '(claw-utils:claw-pointer %filament.util::aw+filament+tools+memio+in-memory-config) config
         '%filament.util::matc+config+output-format :blob)

        (%filament.util:matc+set-optimization-level
         '(claw-utils:claw-pointer %filament.util::matc+config) config
         '%filament.util::matc+config+optimization (or optimization :performance))

        (%filament.util:matc+run '(:pointer %filament.util::matc+material-compiler) compiler
                                 '(:pointer %filament.util::matc+config) config)

        (let* ((out (%filament.util:aw+filament+tools+memio+get-output
                     :const
                     '(:pointer %filament.util::aw+filament+tools+memio+in-memory-config) config)))
          (%filament.util:aw+filament+tools+memio+material-data
           '(:pointer %filament.util::aw+filament+tools+memio+in-memory-output) out))))))


(defun destroy-material-data (data)
  ;; we can't use intricate destroy here because instance was allocated in
  ;; foreign code using unaligend (default) allocation
  (%filament.util:aw+filament+tools+memio+~material-data
   '(:pointer %filament.util:aw+filament+tools+memio+material-data) data))


(defun material-data-pointer (data)
  (%filament.util:aw+filament+tools+memio+data
   '(:pointer %filament.util::aw+filament+tools+memio+material-data) data))


(defun material-data-size (data)
  (%filament.util:aw+filament+tools+memio+size
   '(:pointer %filament.util::aw+filament+tools+memio+material-data) data))


(defmacro with-serialized-material-data ((material-data source &key base-path debug
                                                                 target-api
                                                                 platform
                                                                 optimization)
                                         &body body)
  `(let ((,material-data (serialize-material ,source
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
       (destroy-material-data ,material-data))))
