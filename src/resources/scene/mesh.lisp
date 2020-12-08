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


(defun parse-mesh ()
  (with-mesh (mesh)
    (let ((primitive-types (loop with primitive-config = (mesh :primitive-types)
                                 for key in (cffi:foreign-enum-keyword-list '%assimp:primitive-type)
                                 unless (or
                                         (eq key :force32bit)
                                         (= (logand (primitive-type-enum key) primitive-config) 0))
                                   collect key))
          (vertex-count (mesh :num-vertices))
          (descriptor (append '(:position)
                              (unless (cffi:null-pointer-p (mesh :normals))
                                `(:normal))
                              (unless (cffi:null-pointer-p (mesh :tangents))
                                `(:tangent))
                              (unless (cffi:null-pointer-p (mesh :bitangents))
                                `(:bitangent))
                              (a:when-let ((color-config
                                            (loop for idx below %ai:+max-number-of-color-sets+
                                                  unless (cffi:null-pointer-p
                                                          (mesh :colors idx))
                                                    collect idx)))
                                `((:color :planes ,color-config)))
                              (a:when-let ((uv-config
                                            (loop for idx below %ai:+max-number-of-texturecoords+
                                                  unless (cffi:null-pointer-p
                                                          (mesh :texture-coords idx))
                                                    collect (list idx (mesh :num-uv-components idx)))))
                                `((:uv :planes ,uv-config))))))
      (list primitive-types vertex-count descriptor))))


(defun parse-meshes ()
  (with-scene (scene)
    (loop for mesh-idx below (scene :num-meshes)
          collect (let ((*mesh* (scene :meshes * mesh-idx)))
                    (parse-mesh)))))
