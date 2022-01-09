(cl:in-package :alien-works.framework)


(defun call-with-framework (fu &key (window-title "Alien-Works")
                                 window-width
                                 window-height)
  (handler-bind ((serious-condition (lambda (c)
                                      (format *error-output* "~%Unhandled serious condition:~%")
                                      (dissect:present c *error-output*))))
    (dissect:with-capped-stack ()
      (float-features:with-float-traps-masked t
        (%host:with-window (:title window-title
                            :width window-width
                            :height window-height)
          (%audio:with-audio ()
            (%graphics:with-renderer ()
              (funcall fu))))))))


(defmacro with-alien-works ((&key window-title window-width window-height) &body body)
  `(call-with-framework
    (lambda () ,@body)
    ,@(when window-title
        `(:window-title ,window-title))
    ,@(when window-width
        `(:window-width ,window-width))
    ,@(when window-height
        `(:window-height ,window-height))))
