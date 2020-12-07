(cl:in-package :alien-works.resources)


(declaim (special *scene*
                  *mesh*
                  *material*))


(defmacro with-ai-struct ((var type &optional value) &body body)
  `(cref:c-let ((,var (:struct ,type) :from ,(or value var)))
     ,@body))


(defmacro with-scene ((scene-var) &body body)
  `(with-ai-struct (,scene-var %ai:scene *scene*)
     ,@body))


(defmacro with-mesh ((mesh-var &optional mesh-val) &body body)
  `(with-ai-struct (,mesh-var %ai:mesh (or ,mesh-val *mesh*))
     ,@body))


(defmacro with-material ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:material (or ,value *material*))
     ,@body))


(defmacro with-material-property ((var &optional value) &body body)
  `(with-ai-struct (,var %ai:material-property ,value)
     ,@body))


(defun ai-string-to-lisp (ai-string)
  (cref:c-val ((ai-string (:struct %ai:string)))
    (cffi:foreign-string-to-lisp (ai-string :data &) :count (ai-string :length))))
