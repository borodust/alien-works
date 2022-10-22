(cl:in-package :alien-works.memory)


(defun symbolicate-memory-layout-name (name)
  (u:symbolicate* name '$alien-works$memory-layout))


(defun find-foreign-type (type)
  (let ((layout-type `(:struct
                       ,(symbolicate-memory-layout-name
                         type))))
    (if (ignore-errors (cffi::parse-type layout-type))
        (values layout-type t)
        (if (ignore-errors (cffi::parse-type type))
            (values type nil)
            nil))))


(defmacro define-memory-layout (name-and-opts &body slots)
  (destructuring-bind (name &rest opts) (a:ensure-list name-and-opts)
    (declare (ignore opts))
    (let ((safe-name (symbolicate-memory-layout-name name)))
      `(cffi:defcstruct ,safe-name
         ,@(loop for slot in slots
                 collect (destructuring-bind (name type &rest rest) slot
                           `(,name
                             ,(or (find-foreign-type type) type)
                             ,@rest)))))))


(defun memory-layout-slot-offset (layout slot-name)
  (let* ((type `(:struct ,(symbolicate-memory-layout-name layout)))
         (slot (cref::find-slot-name type slot-name)))
    (cffi:foreign-slot-offset type slot)))


(define-compiler-macro memory-layout-slot-offset (&whole whole layout slot-name)
  (let ((unquoted-layout (u:unquote layout))
        (unquoted-slot-name (u:unquote slot-name)))
    (if (and (not (eq layout unquoted-layout))
             (and (or (keywordp slot-name)
                      (not (eq unquoted-slot-name slot-name)))))
        (let ((type `(:struct ,(symbolicate-memory-layout-name unquoted-layout))))
          `(cffi:foreign-slot-offset
            ,type
            ',(cref::find-slot-name type slot-name)))
        whole)))


(defun memory-layout-size (layout-name)
  (cffi:foreign-type-size `(:struct ,(symbolicate-memory-layout-name layout-name))))


(define-compiler-macro memory-layout-size (&whole whole layout-name)
  (let ((unquoted-layout (u:unquote layout-name)))
    (if (not (eq layout-name unquoted-layout))
        `(cffi:foreign-type-size
          '(:struct
            ,(symbolicate-memory-layout-name unquoted-layout)))
        whole)))


(defmacro access-memory (memory-vector layout &rest accessors)
  `(cref:c-ref (memory-vector-pointer ,memory-vector)
               ',(symbolicate-memory-layout-name (u:unquote layout))
               ,@accessors))


(defmacro with-memory-access ((name memory-vector layout &key offset) &body body)
  `(cref:c-let ((,name (:struct ,(symbolicate-memory-layout-name layout))
                       :from ,(if offset
                                  `(cffi:inc-pointer
                                    (memory-vector-pointer ,memory-vector)
                                    ,offset)
                                  `(memory-vector-pointer ,memory-vector))))
     ,@body))


(defun make-memory-vector (length &key (type :uint8) initial-contents)
  (sv:make-static-vector (* length (find-foreign-type type))
                         :element-type '(unsigned-byte 8)
                         :initial-contents initial-contents))


(define-compiler-macro make-memory-vector (&whole whole length &key (type :uint8) initial-contents)
  (let ((actual-type (find-foreign-type (u:unquote type))))
    (if actual-type
        `(sv:make-static-vector (* ,length ,(cffi:foreign-type-size actual-type))
                                :element-type '(unsigned-byte 8)
                                ,@(when initial-contents
                                    `(:initial-contents ,initial-contents)))
        whole)))


(defun %alien-works.memory:memory-vector-pointer (memory-vector &optional offset)
  (let ((ptr (sv:static-vector-pointer memory-vector)))
    (if offset
        (cffi:inc-pointer ptr offset)
        ptr)))


(define-compiler-macro %alien-works.memory:memory-vector-pointer (memory-vector
                                                                  &optional offset)
  (if offset
      `(cffi:inc-pointer (sv:static-vector-pointer ,memory-vector) ,offset)
      `(sv:static-vector-pointer ,memory-vector)))


(defun destroy-memory-vector (memory-vector)
  (sv:free-static-vector memory-vector))


(defmacro with-memory-vector ((binding length &optional type) &body body)
  `(let ((,binding (make-memory-vector ,length ,@(when type `(',type)))))
     (unwind-protect
          (progn ,@body)
       (destroy-memory-vector ,binding))))


(defmacro with-memory-vectors (bindings &body body)
  (labels ((%expand (bindings)
             (if bindings
                 `(with-memory-vector ,(first bindings)
                    ,(%expand (rest bindings)))
                 `(progn ,@body))))
    (%expand bindings)))
