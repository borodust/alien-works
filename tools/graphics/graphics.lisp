(cl:in-package :alien-works.tools.graphics)


(defun make-material (material-name &key base-path
                                      debug
                                      (target-api :opengl)
                                      (platform :all)
                                      (optimization :performance))
  (let ((source (with-output-to-string (out)
                  (%gx:print-material-source material-name out))))
    (%gxs:with-serialized-material-data (material-data source
                                         :base-path base-path
                                         :debug debug
                                         :target-api target-api
                                         :platform platform
                                         :optimization optimization)
      (%aw.fm:with-material-builder (%make-material
                                     (:package (%gxs:material-data-pointer material-data)
                                               (%gxs:material-data-size material-data)))
        (%make-material (%alien-works.graphics:engine-handle))))))


(defun serialize-material (material-name &key base-path debug
                                           (target-api :opengl)
                                           (platform :all)
                                           (optimization :performance))
  (let ((source (with-output-to-string (out)
                  (%gx:print-material-source material-name out))))
    (%gxs:with-serialized-material-data (material-data source
                                         :base-path base-path
                                         :debug debug
                                         :target-api target-api
                                         :platform platform
                                         :optimization optimization)
      (let ((serialized-mat-data (mem:make-memory-vector
                                  (%gxs:material-data-size material-data))))
        (host:memcpy (%mem:memory-vector-pointer serialized-mat-data)
                     (%gxs:material-data-pointer material-data)
                     (length serialized-mat-data))
        serialized-mat-data))))
