(cl:in-package :%alien-works.filament)


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


(defgeneric builder-option-intricate-function (builder option))


(defun explode-builder (name-and-opts
                        builder-name
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
                   collect (explode-function (builder-option-intricate-function builder-name name)
                                             (list* builder args)))
           (flet ((,name (,@maker-args)
                    ,(funcall build-expander builder)))
             (,@(if instance
                    `(let ((,instance ,builder)))
                    '(progn))
              ,@body)))))))


(defmacro warp-intricate-builder-option (builder option-name intricate-function &body params)
  (let ((intricate-signature (list* intricate-function params)))
    `(progn
       (warp-intricate-function ,(a:symbolicate builder '- option-name) ,@intricate-signature)
       (defmethod builder-option-intricate-function ((builder (eql ',builder))
                                                     (option (eql ,option-name)))
         (declare (ignore builder option))
         ',intricate-signature))))
