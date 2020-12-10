(cl:in-package :alien-works.resources)


(declaim (special *scene*
                  *mesh*
                  *material*))


(a:define-constant +attribute-alignment+ 4)


(defvar *dry-run* nil)

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


(defmacro with-vector3d ((var &optional src) &body body)
  (if src
      `(cref:c-let ((,var (:struct %ai:vector3d) :from ,src))
         ,@body)
      `(cref:c-val ((,var (:struct %ai:vector3d)))
         ,@body)))


(defmacro with-vector3d* ((&rest bindings) &body body)
  (u:expand-multibinding 'with-vector3d bindings body))


(defmacro with-color4d ((var &optional src) &body body)
  (if src
      `(cref:c-let ((,var (:struct %ai:color4d) :from ,src))
         ,@body)
      `(cref:c-val ((,var (:struct %ai:color4d)))
         ,@body)))


(defun ai-string-to-lisp (ai-string)
  (cref:c-val ((ai-string (:struct %ai:string)))
    (cffi:foreign-string-to-lisp (ai-string :data &) :count (ai-string :length))))


(defmacro write-primitives (buffer type values)
  (a:with-gensyms (idx value)
    (a:once-only (buffer)
      `(cref:c-val ((,buffer ,type))
         (loop for ,value in ,values
               for ,idx from 0
               unless *dry-run*
                 do (setf (,buffer ,idx) ,value)
               finally (return (* (cffi:foreign-type-size ,type) (1+ ,idx))))))))


(defun write-float (buffer &rest values)
  (write-primitives buffer :float values))


(defun write-int16 (buffer &rest values)
  (write-primitives buffer :int16 values))


(defun write-int8 (buffer &rest values)
  (write-primitives buffer :int8 values))


(defun normalize-uint8 (float)
  (let ((uint8-size (1- (ash 1 8))))
    (round (* uint8-size float))))


(defun normalize-int16 (float)
  (let ((int16-size (1- (ash 1 15))))
    (round (* int16-size float))))


(defun calc-alignment-padding (bytes)
  (let* ((offset (mod bytes +attribute-alignment+)))
    (if (zerop offset)
        0
        (- +attribute-alignment+ offset))))


(defun align-buffer (buffer)
  (let* ((address (cffi:pointer-address buffer))
         (shift (calc-alignment-padding address)))
    (values (cffi:inc-pointer buffer shift) shift)))


(defmacro dry-run (&body body)
  `(let ((*dry-run* t))
     ,@body))
