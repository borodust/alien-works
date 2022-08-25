(cl:in-package :alien-works.memory)

(defvar iffi:*allocator* #'aligned-alloc)
(defvar iffi:*extricator* #'aligned-free)

;;;
;;; SYSTEM ALLOCATOR
;;;
(defun system-allocator-expander (align-var size-var)
  `(aligned-alloc ,align-var ,size-var))


(defun system-extricator-expander (ptr-var)
  `(aligned-free ,ptr-var))

(setf iffi:*allocator-expander* 'system-allocator-expander
      iffi:*extricator-expander* 'system-extricator-expander)


(defmacro with-system-allocator (() &body body)
  (cltl2:compiler-let (iffi:*allocator-expander*
                       iffi:*extricator-expander*)
    (flet ((%allocator-expander (align-var size-var)
             `(aligned-alloc ,align-var ,size-var))
           (%extricator-expander (ptr-var)
             `(aligned-free ,ptr-var)))
      (setf iffi:*allocator-expander* #'%allocator-expander
            iffi:*extricator-expander* #'%extricator-expander))
    `(let ((iffi:*allocator* #'aligned-alloc)
           (iffi:*extricator* #'aligned-free))
       ,@body)))

;;;
;;; STACK ALLOCATOR
;;;
(defun align-address (address alignment)
  (let ((misaligned (mod address alignment)))
    (if (zerop misaligned)
        address
        (+ address ( - alignment misaligned)))))


(defun align-pointer (ptr alignment)
  (cffi:make-pointer (align-address (cffi:pointer-address ptr) alignment)))


(defstruct (stack-chunk
            (:constructor %make-stack-chunk))
  (memory nil :type (simple-array (unsigned-byte 8) *) :read-only t)
  (head nil :type (unsigned-byte 64))
  (tail nil :type (unsigned-byte 64)))


(defun make-stack-chunk (chunk-size)
  (let* ((mem (sv:make-static-vector chunk-size
                                     :element-type '(unsigned-byte 8)))
         (mem-address (cffi:pointer-address (sv:static-vector-pointer mem))))
    (%make-stack-chunk :memory mem
                       :head mem-address
                       :tail mem-address)))


(defun destroy-stack-chunk (chunk)
  (sv:free-static-vector (stack-chunk-memory chunk)))


(defun stack-chunk-allocate (chunk alignment size)
  (let* ((mem (stack-chunk-memory chunk))
         (head-address (stack-chunk-head chunk))
         (tail-address (stack-chunk-tail chunk))
         (data-address (align-address head-address alignment))
         (next-head-address (+ data-address size)))
    (unless (> next-head-address
               (+ (cffi:pointer-address (sv:static-vector-pointer mem)) (length mem)))
      (when (= tail-address head-address)
        (setf (stack-chunk-tail chunk) data-address))
      (setf (stack-chunk-head chunk) next-head-address)
      (cffi:make-pointer data-address))))


(defun stack-chunk-free (chunk ptr)
  (let ((head-address (stack-chunk-head chunk))
        (tail-address (stack-chunk-head chunk))
        (mem-address (cffi:pointer-address
                      (sv:static-vector-pointer (stack-chunk-memory chunk))))
        (ptr-address (cffi:pointer-address ptr)))
    (when (> ptr-address head-address)
      (error "Pointer address is higher than stack head address (pointer: ~A, head: ~A"
             ptr-address head-address))
    (when (< ptr-address tail-address)
      (error "Pointer address is lower than stack tail address (pointer: ~A, tail: ~A"
             ptr-address tail-address))
    (if (= ptr-address tail-address)
        (setf (stack-chunk-tail chunk) mem-address
              (stack-chunk-head chunk) mem-address)
        (setf (stack-chunk-head chunk) (cffi:pointer-address ptr))))
  (values))


(defun stack-chunk-empty-p (chunk)
  (= (stack-chunk-tail chunk) (stack-chunk-head chunk)))


(defstruct (stack-allocator
            (:constructor %make-stack-allocator))
  (chunk-size 0 :type (unsigned-byte 32) :read-only t)
  (chunks nil :type list))


(defun make-stack-allocator (&key chunk-size initial-chunk-size)
  (let ((chunk-size (or chunk-size (* 16 1024 1024))))
    (%make-stack-allocator :chunk-size chunk-size
                           :chunks (list (make-stack-chunk (or initial-chunk-size
                                                               chunk-size))))))


(defun destroy-stack-allocator (allocator)
  (mapc #'destroy-stack-chunk (stack-allocator-chunks allocator))
  (values))


(defun stack-alloc (allocator alignment size)
  (let ((current-chunk (first (stack-allocator-chunks allocator))))
    (a:if-let ((ptr (stack-chunk-allocate current-chunk alignment size)))
      ptr
      (let ((new-chunk (make-stack-chunk (stack-allocator-chunk-size allocator))))
        (a:if-let ((ptr (stack-chunk-allocate new-chunk alignment size)))
          (prog1 ptr (push new-chunk (stack-allocator-chunks allocator)))
          (progn
            (destroy-stack-chunk new-chunk)
            (error "Cannot fit data of size ~A with alignment ~A" size alignment)))))))


(defun stack-free (allocator ptr)
  (let ((current-chunk (first (stack-allocator-chunks allocator))))
    (stack-chunk-free current-chunk ptr)
    (when (and (stack-chunk-empty-p current-chunk)
               (rest (stack-allocator-chunks allocator)))
      (pop (stack-allocator-chunks allocator))
      (destroy-stack-chunk current-chunk))
    (values)))


(defmacro with-stack-allocator ((allocator) &body body)
  (a:once-only (allocator)
    (a:with-gensyms (%allocate %free)
      (cltl2:compiler-let (iffi:*allocator-expander*
                           iffi:*extricator-expander*)
        (flet ((%allocator-expander (align-var size-var)
                 `(stack-alloc ,allocator ,align-var ,size-var))
               (%extricator-expander (ptr-var)
                 `(stack-free ,allocator ,ptr-var)))
          (setf iffi:*allocator-expander* #'%allocator-expander
                iffi:*extricator-expander* #'%extricator-expander))
        `(flet ((,%allocate (alignment size)
                  (stack-alloc ,allocator alignment size))
                (,%free (ptr)
                  (stack-free ,allocator ptr)))
           (let ((iffi:*allocator* #',%allocate)
                 (iffi:*extricator* #',%free))
             ,@body))))))
