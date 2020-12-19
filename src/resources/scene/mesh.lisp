(cl:in-package :alien-works.resources)


(u:define-enumval-extractor primitive-type-enum %assimp:primitive-type)

;;
;; Adapted from Filament's
;; TQuaternion<T> TMat33<T>::packTangentFrame(const TMat33<T>& m, size_t storageSize)
;;
(defun pack-tangent-frame (quat-result tangent bitangent normal)
  (m:with-mat3-from-basis (origin tangent bitangent normal)
    (m:with-vec3 (crossed)
      (m:vec3-cross crossed normal tangent)
      (m:with-mat3-from-basis (quatmat tangent crossed normal)
        (m:mat3->quat quat-result quatmat)
        (m:positivize-quat quat-result (m:normalize-quat quat-result quat-result))

        ;; Ensure w is never 0.0
        ;; Bias is 2^(nb_bits - 1) - 1
        ;; NOTE: using 16 bits for SNORM16
        (let ((bias (/ 1f0 (1- (ash 1 (1- 16))))))
          (when (< (m:quat quat-result 3) bias)
            (let ((factor (sqrt (- 1f0 (* bias bias)))))
              (setf (m:quat quat-result 3) bias

                    (m:quat quat-result 0) (* (m:quat quat-result 0) factor)
                    (m:quat quat-result 1) (* (m:quat quat-result 1) factor)
                    (m:quat quat-result 2) (* (m:quat quat-result 2) factor)))))
        ;; If there's a reflection ((n x t) . b <= 0), make sure w is negative
        (m:with-vec3 (crossed)
          (m:vec3-cross crossed tangent normal)
          (when (< (m:vec3-dot crossed bitangent) 0)
            (m:negate-quat quat-result quat-result))))))
  quat-result)


(defun write-position (buffer pos)
  (with-vector3d (pos)
    (multiple-value-bind (buffer shift) (align-buffer buffer)
      (values (+ shift (write-float buffer (pos :x) (pos :y) (pos :z)))
              `(,@(unless (zerop shift)
                    `((:offset :byte ,shift)))
                (:position :float 3))))))


(defun write-quat-tangent (buffer a-normal a-tangent a-bitangent)
  (with-vector3d* ((a-normal)
                   (a-tangent)
                   (a-bitangent))
    (m:with-vec3* ((normal :x (a-normal :x)
                           :y (a-normal :y)
                           :z (a-normal :z))
                   (tangent :x (a-tangent :x)
                            :y (a-tangent :y)
                            :z (a-tangent :z))
                   (bitangent :x (a-bitangent :x)
                              :y (a-bitangent :y)
                              :z (a-bitangent :z)))
      (m:with-quat (result)
        (pack-tangent-frame result tangent bitangent normal)
        (multiple-value-bind (buffer shift) (align-buffer buffer)
          (values
           (+ shift (write-int16 buffer
                                 (normalize-int16 (m:quat result 0))
                                 (normalize-int16 (m:quat result 1))
                                 (normalize-int16 (m:quat result 2))
                                 (normalize-int16 (m:quat result 3))))
           `(,@(unless (zerop shift)
                 `((:offset :byte ,shift)))
             (:tangent :short 4 :normalized t))))))))


(defun write-color (buffer col)
  (with-color4d (col)
    (multiple-value-bind (buffer shift) (align-buffer buffer)
      (values
       (+ shift (write-int8 buffer
                            (normalize-uint8 (col :r))
                            (normalize-uint8 (col :g))
                            (normalize-uint8 (col :b))
                            (normalize-uint8 (col :a))))
       `(,@(unless (zerop shift)
             `((:offset :byte ,shift)))
         (:color :ubyte 4 :normalized t))))))


(defun write-uv (buffer uv channels)
  (with-vector3d (uv)
    (multiple-value-bind (buffer shift) (align-buffer buffer)
      (values
       (+ shift
          (ecase channels
            (1 (write-int16 buffer
                            (normalize-int16 (uv :x))))
            (2 (write-int16 buffer
                            (normalize-int16 (uv :x))
                            (normalize-int16 (uv :y))))
            (3 (write-int16 buffer
                            (normalize-int16 (uv :x))
                            (normalize-int16 (uv :y))
                            (normalize-int16 (uv :z))))))
       `(,@(unless (zerop shift)
             `((:offset :byte ,shift)))
         (:uv :short ,channels :normalized t))))))


(defun write-face (buffer face)
  (with-face (face)
    (loop with ptr = buffer
          for idx below (face :num-indices)
          for written = (write-uint32 ptr (face :indices * idx))
          do (setf ptr (cffi:inc-pointer ptr written))
          finally (return ptr))))


(defun combine-binary-writers (&rest writers)
  (lambda (buffer idx)
    (multiple-value-bind (buffer shift) (align-buffer buffer)
      (multiple-value-bind (written descriptors)
          (loop with ptr = buffer
                with total = 0
                for writer in writers
                for (written descriptor) = (multiple-value-list (funcall writer ptr idx))
                do (setf ptr (cffi:inc-pointer ptr written))
                   (incf total written)
                append descriptor into descriptors
                finally (return (values total descriptors)))
        (let* ((total (+ shift written))
               (padding (calc-alignment-padding total)))
          (values total
                  (append (unless (zerop shift)
                            `((:offset :byte ,shift)))
                          descriptors
                          (unless (zerop padding)
                            `((:offset :byte ,padding))))))))))


(defclass buffer ()
  ((data :initarg :data :initform (error ":data missing") :reader buffer-data)
   (size :initarg :size :initform (error ":size missing") :reader buffer-size)
   (descriptor :initarg :descriptor :initform (error ":descriptor missing") :reader buffer-descriptor)))


(defun destroy-buffer (buffer)
  (with-slots (data) buffer
    (cffi:foreign-free data)))


(defclass mesh ()
  ((vertex-buffer :initarg :vertex-buffer
                  :initform (error ":vertex-buffer missing")
                  :reader mesh-vertex-buffer)
   (index-buffers :initarg :index-buffers
                  :initform nil
                  :reader mesh-index-buffers)
   (material :initarg :material
             :initform nil
             :reader mesh-material)
   (aabb :initarg :aabb
         :initform nil
         :reader mesh-aabb)))


(defun destroy-mesh (mesh)
  (with-slots (vertex-buffer index-buffers) mesh
    (destroy-buffer vertex-buffer)
    (loop for buf in index-buffers
          do (destroy-buffer buf))))


(defun parse-vertices ()
  (with-mesh (mesh)
    (let* ((vertex-count (mesh :num-vertices))
           (writers (append (list
                             (lambda (buffer idx)
                               (write-position buffer (mesh :vertices * idx &))))
                            (unless (or (cffi:null-pointer-p (mesh :normals))
                                        (cffi:null-pointer-p (mesh :tangents))
                                        (cffi:null-pointer-p (mesh :bitangents)))
                              (list
                               (lambda (buffer idx)
                                 (write-quat-tangent buffer
                                                     (mesh :normals * idx &)
                                                     (mesh :tangents * idx &)
                                                     (mesh :bitangents * idx &)))))
                            (a:when-let ((color-indices
                                          (loop for idx below %ai:+max-number-of-color-sets+
                                                unless (cffi:null-pointer-p (mesh :colors idx))
                                                  collect idx)))
                              (loop for color-idx in color-indices
                                    collect (lambda (buffer idx)
                                              (write-color buffer (mesh :colors color-idx * idx &)))))
                            (a:when-let ((uv-config
                                          (loop for idx below %ai:+max-number-of-texturecoords+
                                                unless (cffi:null-pointer-p
                                                        (mesh :texture-coords idx))
                                                  collect (list idx (mesh :num-uv-components idx)))))
                              (loop for (uv-idx component-count) in uv-config
                                    collect (lambda (buffer idx)
                                              (write-uv buffer
                                                        (mesh :texture-coords uv-idx * idx &)
                                                        component-count))))))
           (uber-writer (apply #'combine-binary-writers writers)))
      (unless (zerop vertex-count)
        (multiple-value-bind (vert-len descriptor)
            (dry-run (funcall uber-writer (cffi:null-pointer) 0))
          (let* ((buffer-size (* vert-len vertex-count))
                 (buffer (cffi:foreign-alloc :int8 :count buffer-size)))
            (loop for idx below vertex-count
                  do (funcall uber-writer (cffi:inc-pointer buffer (* idx vert-len)) idx))
            (make-instance 'buffer :data buffer
                                   :size buffer-size
                                   :descriptor descriptor)))))))


(defun parse-faces ()
  (flet ((%next-primitive (faces max-count)
           (if (> max-count 0)
               (cref:c-val ((faces (:struct %assimp:face)))
                 (loop with face-size = (faces 0 :num-indices)
                       for idx from 0 below max-count
                       while (= face-size (faces idx :num-indices))
                       finally (return (list face-size idx))))
               (list 0 0))))
    (with-mesh (mesh)
      (let ((total-face-count (mesh :num-faces)))
        (unless (zerop total-face-count)
          (loop with faces-ptr = (mesh :faces)
                with rest-count = total-face-count
                for (next-face-size next-face-count) = (%next-primitive faces-ptr rest-count)
                while (> next-face-count 0)
                collect (let* ((buffer-size (* next-face-count
                                               next-face-size
                                               (cffi:foreign-type-size :uint32)))
                               (buffer (cffi:foreign-alloc :int8 :count buffer-size))
                               (primitive (ecase next-face-size
                                            (1 :points)
                                            (2 :lines)
                                            (3 :triangles))))
                          (with-face (next-faces-ptr faces-ptr)
                            (loop with ptr = buffer
                                  for face-idx below next-face-count
                                  for next-face-ptr = (next-faces-ptr face-idx &)
                                  do (setf ptr (write-face ptr next-face-ptr))
                                  finally (setf faces-ptr next-face-ptr
                                                rest-count (- rest-count next-face-count))))
                          (make-instance 'buffer :data buffer
                                                 :size buffer-size
                                                 :descriptor `((:index :uint 1
                                                                       :primitive ,primitive))))))))))

(defun parse-mesh ()
  (make-instance 'mesh
                 :vertex-buffer (parse-vertices)
                 :index-buffers (parse-faces)))


(defun parse-meshes ()
  (with-scene (scene)
    (loop for mesh-idx below (scene :num-meshes)
          collect (let ((*mesh* (scene :meshes * mesh-idx)))
                    (parse-mesh)))))
