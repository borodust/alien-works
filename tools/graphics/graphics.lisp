(cl:in-package :alien-works.tools.graphics)


(defun make-material (source &key base-path)
  (%gxs:with-parsed-material (material source :base-path base-path)
    (%gx:with-material-builder (%make-material
                                (:package (%gxs:material-data material)
                                          (%gxs:material-size material)))
      (%make-material (%alien-works.graphics:engine-handle)))))


(defun parse-material (source &key base-path debug
                                (target-api :all)
                                (platform :all)
                                (optimization :performance))
  (let ((material (%gxs:parse-material source :base-path base-path
                                              :debug debug
                                              :target-api target-api
                                              :platform platform
                                              :optimization optimization)))
    (values (%gxs:material-data material) (%gxs:material-size material))))
