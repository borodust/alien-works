(cl:defpackage :alien-works.utils.empty
  (:use))

(cl:defpackage :alien-works.utils
  (:local-nicknames (:a :alexandria))
  (:use :cl)
  (:export #:enumval
           #:define-enumval-extractor))
(cl:in-package :alien-works.utils)


(defun enumval (enum value)
  (cffi:foreign-enum-value enum value))


(define-compiler-macro enumval (&whole whole enum value)
  (a:if-let ((quoted (when (and (listp enum) (eq 'quote (first enum)))
                       (second enum))))
    (if (keywordp value)
        (a:if-let ((value (cffi:foreign-enum-value quoted value :errorp nil)))
          value
          whole)
        whole)
    whole))


(defmacro define-enumval-extractor (name enum)
  `(progn
     (defun ,name (value)
       (enumval ',enum value))
     (define-compiler-macro ,name (value)
       (enumval ',enum value))))
