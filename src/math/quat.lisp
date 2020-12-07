(cl:in-package :alien-works.math)


(defun quat (quat idx)
  (cffi:mem-ref (%glm:glm-operator[] '(:pointer %glm:glm-quat) quat :int idx) :float))


(defun (setf quat) (value quat idx)
  (let ((ptr (%glm:glm-operator[] '(:pointer %glm:glm-quat) quat :int idx)))
    (setf (cffi:mem-ref ptr :float) (float value 0f0))))


(defun make-quat (x y z w)
  (iffi:make-intricate-instance '%glm:glm-quat
                                :float (float x 0f0)
                                :float (float y 0f0)
                                :float (float z 0f0)
                                :float (float w 0f0)))


(defun mat3->quat (result mat3)
  (%glm:glm-quat-cast
   '(:pointer %glm:glm-quat) result
   '(:pointer %glm:glm-mat3) mat3))


(defun destroy-quat (quat)
  (iffi:destroy-intricate-instance '%glm:glm-quat quat))


(defmacro with-quat ((quat &key (x 0f0) (y 0f0) (z 0f0) (w 1f0)) &body body)
  `(let ((,quat (make-quat ,x ,y ,z ,w)))
     (unwind-protect
          (progn ,@body)
       (destroy-quat ,quat))))


(defmacro with-quat-from-mat3 ((quat mat3) &body body)
  `(let ((,quat (mat3->quat (iffi:make-intricate-instance '%glm:glm-quat) ,mat3)))
     (unwind-protect
          (progn ,@body)
       (destroy-quat, quat))))


(defun normalize-quat (result quat)
  (%glm:glm-normalize
   '(:pointer %glm:glm-quat) result
   '(:pointer %glm:glm-quat) quat))


(defun copy-quat (result quat)
  (%glm:glm-operator=
   '(:pointer %glm:glm-quat) result
   '(:pointer %glm:glm-quat) quat))


(defun negate-quat (result quat)
  (%glm:glm-operator-
     '(:pointer %glm:glm-quat) result
     '(:pointer %glm:glm-quat) quat))


(defun positivize-quat (result quat)
  (unless (cffi:pointer-eq result quat)
    (copy-quat result quat))
  (when (< (quat result 3) 0f0)
    (negate-quat result quat))
  result)
