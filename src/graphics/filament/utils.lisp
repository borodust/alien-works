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


(defmacro warp-intricate-function (name intricate-name &body params)
  (let ((args (loop for nil in params
                    collect (gensym))))
    `(defun ,name (,@args)
       ,(explode-function (list* intricate-name params) args))))


(defun explode-builder (name-and-opts
                        opt-expander
                        ctor-expander
                        build-expander
                        maker-args
                        steps
                        body)
  (a:with-gensyms (builder)
    (destructuring-bind (name &rest opts) (a:ensure-list name-and-opts)
      (destructuring-bind (&key instance &allow-other-keys) opts
        `(iffi:with-intricate-instance (,builder ,@(funcall ctor-expander))
           ,@(loop for (name . args) in steps
                   collect (explode-function (funcall opt-expander name) (list* builder args)))
           (flet ((,name (,@maker-args)
                    ,(funcall build-expander builder)))
             (,@(if instance
                    `(let ((,instance ,builder)))
                    '(progn))
              ,@body)))))))
