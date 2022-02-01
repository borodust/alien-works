(cl:in-package :alien-works.tools.resources)

;;;
;;; MATERIAL PROVIDER
;;;
(defun make-material-provider ()
  (%filament:gltfio+create-material-generator
   '(claw-utils:claw-pointer %filament::engine) (%alien-works.graphics:engine-handle)
   :bool t))


(defun destroy-material-provider (provider)
  (%filament:gltfio+~material-provider
   '(claw-utils:claw-pointer %filament::gltfio+material-provider) provider))


;;;
;;; GLTF LOADER
;;;
(defun make-gltf-loader (material-provider)
  (iffi:with-intricate-instance (cfg %filament:gltfio+asset-configuration)
    (iffi:with-intricate-slots %filament:gltfio+asset-configuration
        ((cfg-engine %filament:engine)
         (cfg-materials %filament:materials))
        cfg
      (setf cfg-engine (%alien-works.graphics:engine-handle)
            cfg-materials material-provider))

    (%filament:gltfio+asset-loader+create
     '(claw-utils:claw-pointer %filament:gltfio+asset-configuration) cfg)))


(defun destroy-gltf-loader (loader)
  (cref:c-with ((ptr :pointer))
    (setf ptr loader)
    (%filament:gltfio+asset-loader+destroy
     '(claw-utils:claw-pointer (claw-utils:claw-pointer %filament::gltfio+asset-loader)) (ptr &))))


(defun load-text-gltf-model-from-byte-vector (loader data)
  (u:with-pinned-array-pointer (ptr data)
    (%filament:gltfio+create-asset-from-json
     '(claw-utils:claw-pointer %filament::gltfio+asset-loader) loader
     '(claw-utils:claw-pointer %filament::uint8-t) ptr
     '%filament::uint32-t (length data))))


(defun load-binary-gltf-model-from-byte-vector (loader data)
  (u:with-pinned-array-pointer (ptr data)
    (%filament:gltfio+create-asset-from-binary
     '(claw-utils:claw-pointer %filament::gltfio+asset-loader) loader
     '(claw-utils:claw-pointer %filament::uint8-t) ptr
     '%filament::uint32-t (length data))))


(defun load-text-gltf-model (loader path)
  (let ((data (host:read-host-file-into-static-vector path :element-type '(unsigned-byte 8))))

    (unwind-protect
         (load-text-gltf-model-from-byte-vector loader data)
      (sv:free-static-vector data))))


(defun load-binary-gltf-model (loader path)
  (let ((data (host:read-host-file-into-static-vector path :element-type '(unsigned-byte 8))))
    (unwind-protect
         (load-binary-gltf-model-from-byte-vector loader data)
      (sv:free-static-vector data))))


(defun destroy-gltf-model (loader asset)
  (%filament:gltfio+destroy-asset
   '(claw-utils:claw-pointer %filament::gltfio+asset-loader) loader
   '(claw-utils:claw-pointer %filament::gltfio+filament-asset) asset))


(defun add-scene-gltf-model (scene asset)
  (let ((entities (%filament:gltfio+get-entities
                   :const
                   '(claw-utils:claw-pointer %filament::gltfio+filament-asset) asset))
        (count (%filament:gltfio+get-entity-count
                :const
                '(claw-utils:claw-pointer %filament::gltfio+filament-asset) asset)))
    (%filament:add-entities
     '(claw-utils:claw-pointer %filament::scene) (alien-works.graphics::handle-of scene)
     '(claw-utils:claw-pointer %filament::utils+entity) entities
     '%filament::size-t count)))


(defun remove-scene-gltf-model (scene asset)
  (let ((entities (%filament:gltfio+get-entities
                   :const
                   '(claw-utils:claw-pointer %filament::gltfio+filament-asset) asset))
        (count (%filament:gltfio+get-entity-count
                :const
                '(claw-utils:claw-pointer %filament::gltfio+filament-asset) asset)))
    (%filament:remove-entities
     '(claw-utils:claw-pointer %filament::scene) (alien-works.graphics::handle-of scene)
     '(claw-utils:claw-pointer %filament::utils+entity) entities
     '%filament::size-t count)))


(defun gltf-model-resource-names (asset)
  (let ((resource-uri-count (%filament:gltfio+get-resource-uri-count
                             :const
                             '(claw-utils:claw-pointer %filament::gltfio+filament-asset) asset))
        (resources-uris (%filament:gltfio+get-resource-uris
                         :const
                         '(claw-utils:claw-pointer %filament::gltfio+filament-asset) asset)))
    (cref:c-val ((resources-uris (:pointer :string)))
      (loop for i from 0 below resource-uri-count
            collect (resources-uris i)))))


;;;
;;; RESOURCE LOADER
;;;
(defun make-gltf-resource-loader ()
  (iffi:with-intricate-instance (cfg %filament:gltfio+resource-configuration)
    (iffi:with-intricate-slots %filament:gltfio+resource-configuration
        ((cfg-engine %filament:engine))
        cfg
      (setf cfg-engine (%alien-works.graphics:engine-handle)))

    (iffi:make-intricate-instance
     '%filament:gltfio+resource-loader
     '(claw-utils:claw-pointer %filament::gltfio+resource-configuration) cfg)))


(defun destroy-gltf-resource-loader (loader)
  (iffi:destroy-intricate-instance '%filament:gltfio+resource-loader loader))


(defun load-gltf-model-resources (loader asset)
  (%filament:gltfio+load-resources
   '(claw-utils:claw-pointer %filament::gltfio+resource-loader) loader
   '(claw-utils:claw-pointer %filament::gltfio+filament-asset) asset))


;;;
;;;
;;;
