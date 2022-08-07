(cl:in-package :alien-works.tools.graphics)


(defun make-material (material-name &key base-path
                                      debug
                                      (target-api :all)
                                      (platform :all)
                                      (optimization :performance))
  (let ((source (with-output-to-string (out)
                  (%gx:print-material-source material-name out))))
    (%gxs:with-parsed-material (material source
                                :base-path base-path
                                :debug debug
                                :target-api target-api
                                :platform platform
                                :optimization optimization)
      (%aw.fm:with-material-builder (%make-material
                                     (:package (%gxs:material-data material)
                                               (%gxs:material-size material)))
        (%make-material (%alien-works.graphics:engine-handle))))))


(defun parse-material (material-name &key base-path debug
                                       (target-api :all)
                                       (platform :all)
                                       (optimization :performance))
  (let* ((source (with-output-to-string (out)
                   (%gx:print-material-source material-name out)))
         (material (%gxs:parse-material source :base-path base-path
                                               :debug debug
                                               :target-api target-api
                                               :platform platform
                                               :optimization optimization)))
    (values (%gxs:material-data material) (%gxs:material-size material))))
