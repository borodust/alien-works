(cl:defpackage :alien-works.utils.empty
  (:use))
(cl:defpackage :alien-works.utils
  (:local-nicknames (:a :alexandria))
  (:use :cl)
  (:export #:enumval
           #:define-enumval-extractor
           #:enumbit
           #:define-enumbit-combiner

           #:expand-multibinding

           #:memcpy
           #:memset))
(cl:in-package :alien-works.utils)


(defun enumval (enum value)
  (if (integerp value)
      (cffi:foreign-enum-keyword enum value)
      (cffi:foreign-enum-value enum value)))


(define-compiler-macro enumval (&whole whole enum value)
  (a:if-let ((quoted (when (and (listp enum)
                                (eq 'quote (first enum)))
                       (second enum))))
    (cond
      ((keywordp value)
       (a:if-let ((value (cffi:foreign-enum-value quoted value :errorp nil)))
         value
         whole))
      ((integerp value)
       (a:if-let ((value (cffi:foreign-enum-keyword quoted value :errorp nil)))
         value
         whole))
      (t whole))
    whole))


(defmacro define-enumval-extractor (name enum)
  `(progn
     (defun ,name (value)
       (enumval ',enum value))
     (define-compiler-macro ,name (value)
       `(enumval ',',enum ,value))))


(defun enumbit (enum &rest names)
  (loop with result = #x0
        for name in names
        for value = (cffi:foreign-enum-value enum name)
        do (setf result (logior result value))
        finally (return result)))


(define-compiler-macro enumbit (&whole whole enum &rest names)
  (if (and (every #'keywordp names)
           (listp enum)
           (eq 'quote (first enum))
           (symbolp (second enum)))
      (apply #'enumbit (second enum) names)
      whole))


(defmacro define-enumbit-combiner (name enum)
  (flet ((%expand-enumbit (names)
           `(apply #'enumbit ',enum ,names)))
    (a:with-gensyms (names)
      `(progn
         (defun ,name (&rest names)
           (apply #'enumbit ',enum names))
         (define-compiler-macro ,name (&rest names)
           (let ((,names names))
             ,(%expand-enumbit names)))))))



(defun expand-multibinding (name bindings body)
  (labels ((%expand (bindings)
             (if bindings
                 `((,name ,(first bindings)
                          ,@(%expand (rest bindings))))
                 body)))
    (first (%expand bindings))))


;; FIXME: remove, newer CFFI has it
(cffi:defctype :size-t :unsigned-int)

;; void * memcpy ( void * destination, const void * source, size_t num );
(cffi:defcfun memcpy :pointer
  (destination :pointer)
  (source :pointer)
  (size :size-t))


;; void * memset ( void * ptr, int value, size_t num );
(cffi:defcfun memset :pointer
  (destination :pointer)
  (value :int)
  (size :size-t))
