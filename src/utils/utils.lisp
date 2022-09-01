#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "sb-cltl2"))

(cl:defpackage :alien-works.utils.empty
  (:use))

(defpackage #:alien-works.cltl2
  (:use #:cl
        #+sbcl #:sb-cltl2
        #+lispworks #:hcl
        #+ecl #:si
        #+openmcl #:ccl)
  #+lispworks (:import-from #:lw #:compiler-let)
  (:export #:compiler-let))

(cl:defpackage :alien-works.utils
  (:local-nicknames (:a :alexandria)
                    (:sv :static-vectors))
  (:use :cl)
  (:export #:enumval
           #:define-enumval-extractor
           #:enumbit
           #:define-enumbit-combiner
           #:define-case-converter

           #:expand-multibinding
           #:definline

           #:with-pinned-array-pointer

           #:read-file-into-shareable-vector
           #:unload-foreign-libraries
           #:reload-foreign-libraries

           #:define-umbrella-package
           #:init-system-allocation-routines

           #:register-foreign-callback
           #:perform-foreign-callback

           #:unquote
           #:symbolicate*
           #:without-float-traps))

(cl:in-package :alien-works.utils)


(defvar *unloaded-foreign-libraries* nil)

(defvar *next-callback-id* 0)
(defvar *callback-table* (make-hash-table :test 'eql))


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
  (a:with-gensyms (names)
    `(progn
       (defun ,name (&rest ,names)
         (apply #'enumbit ',enum ,names))
       (define-compiler-macro ,name (&rest ,names)
         (list* 'alien-works.utils:enumbit '',enum ,names)))))


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


(defun try-static-vector-pointer (data)
  (ignore-errors
   (static-vectors:static-vector-pointer data)))


(defun try-shareable-vector-pointer (data)
  (ignore-errors
   (cffi:with-pointer-to-vector-data (ptr data)
     ptr)))


(defmacro with-pinned-array-pointer ((ptr data &key try-pinned-copy)
                                     &body body)
  (a:with-gensyms (body-fu tmp-vec)
    (a:once-only (data)
      `(flet ((,body-fu (,ptr)
                ,@body))
         (a:if-let (,ptr (try-static-vector-pointer ,data))
           (funcall #',body-fu ,ptr)
           (if (try-shareable-vector-pointer ,data)
               (cffi:with-pointer-to-vector-data (,ptr ,data)
                 (funcall #',body-fu ,ptr))
               ,(if try-pinned-copy
                    `(sv:with-static-vector (,tmp-vec
                                             (length ,data)
                                             :element-type (array-element-type ,data)
                                             :initial-contents ,data)
                       (funcall #',body-fu (sv:static-vector-pointer ,tmp-vec)))
                    '(error "Failed to pin the array"))))))))


(defun unload-foreign-libraries ()
  (bodge-blobs-support:close-foreign-libraries)
  (handler-bind ((style-warning #'muffle-warning))
    (loop for lib in (cffi:list-foreign-libraries :loaded-only t)
          do (progn
               (pushnew (cffi:foreign-library-name lib) *unloaded-foreign-libraries*
                        :test #'equal)
               (cffi:close-foreign-library lib)))))


(defun reload-foreign-libraries ()
  (bodge-blobs-support:load-foreign-libraries)
  (loop for lib-name in *unloaded-foreign-libraries*
        do (cffi:load-foreign-library lib-name))
  (setf *unloaded-foreign-libraries* nil))


(defmacro define-umbrella-package (name &rest packages)
  (let ((existing-package (find-package name)))
    (when existing-package
      (do-symbols (sym existing-package)
        (unintern sym existing-package))))
  (let (import-from)
    (loop for name in packages
          for imported-package = (find-package name)
          do (let (package-symbols)
               (do-external-symbols (sym imported-package)
                 (push (make-symbol (string sym)) package-symbols))
               (push (cons name package-symbols) import-from)))
    `(defpackage ,name
       (:use)
       ,@(loop for (name . symbols) in import-from
               collect `(:import-from ,name ,@symbols))
       (:export ,@(reduce #'union (mapcar #'cdr import-from))))))


(uiop:register-image-dump-hook 'unload-foreign-libraries)


(defmacro init-system-allocation-routines (allocator extricator)
  `(progn
     (declaim (inline aligned-alloc aligned-free))
     ,@(cond
         ((cffi:foreign-symbol-pointer "aligned_alloc")
          `((cffi:defcfun ("aligned_alloc" ,allocator) :pointer
              (byte-alignment :size)
              (byte-size :size))

            (defun ,extricator (ptr)
              (cffi:foreign-free ptr))))

         ((cffi:foreign-symbol-pointer "_aligned_malloc")
          `((declaim (inline %aligned-malloc))
            (cffi:defcfun ("_aligned_malloc" %aligned-malloc) :pointer
              (byte-size :size)
              (byte-alignment :size))

            (defun ,allocator (alignment size)
              (%aligned-malloc size alignment))

            (cffi:defcfun ("_aligned_free" ,extricator) :pointer
              (memory :pointer))))

         (t (error "Aligned memory allocation function not found. No C std library linked?")))))


(defun unquote (expr)
  (if (atom expr)
      expr
      (if (eq 'quote (first expr))
          (second expr)
          expr)))


(defun symbolicate* (symbol &rest rest-symbols)
  (let ((*package* (symbol-package symbol)))
    (apply #'a:symbolicate symbol rest-symbols)))


(defun register-foreign-callback (action)
  (let ((id (setf *next-callback-id* (mod (1+ *next-callback-id*) 4294967295))))
    (setf (gethash id *callback-table*) action)
    id))


(defun perform-foreign-callback (id)
  (a:if-let ((callback (gethash id *callback-table*)))
    (unwind-protect
         (funcall callback)
      (remhash id *callback-table*))
    (error "Callback with id ~A not found" id)))


(defmacro without-float-traps (&body body)
  `(float-features:with-float-traps-masked t
     ,@body))
