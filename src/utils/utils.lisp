(cl:defpackage :alien-works.utils.empty
  (:use))
(cl:defpackage :alien-works.utils
  (:local-nicknames (:a :alexandria))
  (:use :cl)
  (:export #:enumval
           #:define-enumval-extractor
           #:enumbit
           #:define-enumbit-combiner
           #:define-case-converter

           #:expand-multibinding
           #:definline

           #:read-file-into-shareable-vector))
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
           (list* 'enumbit `(quote ,name) names)))
    (a:with-gensyms (names)
      `(progn
         (defun ,name (&rest ,names)
           (apply #'enumbit ',enum ,names))
         (define-compiler-macro ,name (&rest ,names)
           (list* 'alien-works.utils:enumbit '',enum ,names))))))


(defun expand-multibinding (name bindings body)
  (labels ((%expand (bindings)
             (if bindings
                 `((,name ,(a:ensure-list (first bindings))
                          ,@(%expand (rest bindings))))
                 body)))
    (first (%expand bindings))))


(defmacro definline (name (&rest lambda-list) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       ,@body)))


(defmacro define-case-converter ((this-type that-type &key otherwise)
                                 &body pairs)
  (a:with-gensyms (value whole)
    (let ((this->that (a:symbolicate this-type '-> that-type))
          (that->this (a:symbolicate that-type '-> this-type))
          (otherwise (when otherwise
                       (destructuring-bind (this-otherwise
                                            &optional (that-otherwise nil that-provided-p))
                           (a:ensure-list otherwise)
                         (list this-otherwise (if that-provided-p that-otherwise this-otherwise))))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defun ,this->that  (,value)
             (case ,value
               ,@(loop for (this that) in pairs
                       collect `(,this ,that))
               ,(if otherwise
                    `(otherwise ,(first otherwise))
                    `(otherwise ,value))))
           (defun ,that->this (,value)
             (case ,value
               ,@(loop for (this that) in pairs
                       collect `(,that ,this))
               ,(if otherwise
                    `(otherwise ,(second otherwise))
                    `(otherwise ,value)))))
         (define-compiler-macro ,this->that (&whole ,whole ,value)
           (if (or (keywordp ,value) (integerp ,value))
               (,this->that ,value)
               ,whole))
         (define-compiler-macro ,that->this (&whole ,whole ,value)
           (if (or (keywordp ,value) (integerp ,value))
               (,that->this ,value)
               ,whole))))))


(defun read-file-into-shareable-vector (location &key ((:into provided-shareable-vector))
                                                   offset ((:size provided-size)))
  (when (and provided-shareable-vector
             provided-size
             (> provided-size (length provided-shareable-vector)))
    (error "Provided size is smaller than length of provided shareable vector"))
  (with-open-file (stream location :direction :input :element-type '(unsigned-byte 8))
    (let* ((file-size (file-length stream))
           (offset (if (> file-size 0)
                       (mod (or offset 0) file-size)
                       0))
           (rest-file-size (- file-size offset))
           (calculated-size
             (min rest-file-size
                  (or provided-size rest-file-size)
                  (or (and provided-shareable-vector (length provided-shareable-vector))
                      rest-file-size))))
      (when (> (+ offset (or provided-size 0)) file-size)
        (error "Sum of offset and provided size is greater than size of the ~A: got ~A, expected no more than ~A"
               location (+ offset calculated-size) file-size))
      (file-position stream offset)
      (let* ((out (if provided-shareable-vector
                      provided-shareable-vector
                      (cffi:make-shareable-byte-vector calculated-size))))
        (read-sequence out stream :start 0 :end calculated-size)
        (values out file-size)))))
