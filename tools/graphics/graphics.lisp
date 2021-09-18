(cl:in-package :alien-works.tools.graphics)


(defun make-material (engine source &optional base-path)
  (%gxs:with-parsed-material (material source base-path)
    (%gx:with-material-builder (%make-material
                                (:package (%gxs:material-data material)
                                          (%gxs:material-size material)))
      (%make-material (gx::handle-of engine)))))


(defun parse-material (source &optional base-path)
  (let ((material (%gxs:parse-material source base-path)))
    (values (%gxs:material-data material) (%gxs:material-size material))))
