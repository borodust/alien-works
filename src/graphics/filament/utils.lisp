(cl:in-package :%alien-works.graphics)


;;;
;;; BUILDER
;;;
(defun explode-function (signature args)
  (let ((name (first signature))
        (types (rest signature)))
    (when (/= (length types) (length args))
      (error "Wrong number of arguments for ~A: required ~A, but got ~A"
             name types args))
    `(,name ,@(loop for type in types
                    for arg in args
                    append (list type arg)))))


(defun explode-builder (name-and-opts
                        opt-expander
                        ctor-expander
                        build-expander
                        maker-args
                        steps
                        body)
  (a:with-gensyms (builder)
    (let ((name (first (a:ensure-list name-and-opts))))
      `(flet ((,name (,@maker-args)
                (iffi:with-intricate-instance (,builder ,@(funcall ctor-expander))
                  ,@(loop for (name . args) in steps
                          collect (funcall opt-expander name (list* builder args)))
                  ,(funcall build-expander builder))))
         ,@body))))
