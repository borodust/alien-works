(cl:in-package :alien-works.tools.resources)


;;;
;;; LOGGING
;;;
(cffi:defcallback write-assimp-log :void ((message :pointer) (data :pointer))
  (declare (ignore data))
  (format *standard-output* "~A" (cffi:foreign-string-to-lisp message)))


(defmacro with-logging (() &body body)
  (a:with-gensyms (logger)
    `(cref:c-with ((,logger (:struct %ai:log-stream)))
       (setf (,logger :callback) (cffi:callback write-assimp-log))
       (%ai:attach-log-stream (,logger &))
       (unwind-protect
            (progn ,@body)
         (%ai:detach-log-stream (,logger &))))))


;;;
;;; SCENE
;;;
(u:define-enumbit-combiner post-process-steps-bit %assimp:post-process-steps)


(defun call-with-scene (path callback)
  (with-logging ()
    (let* ((*scene* (%ai:import-file (namestring (truename path))
                                     (logior %ai:+process-preset-target-realtime-max-quality+
                                             (post-process-steps-bit :optimize-graph
                                                                     :find-instances
                                                                     :optimize-meshes
                                                                     :calc-tangent-space
                                                                     :gen-smooth-normals
                                                                     :gen-uv-coords
                                                                     :debone
                                                                     :sort-by-p-type
                                                                     :join-identical-vertices
                                                                     :triangulate
                                                                     :improve-cache-locality
                                                                     :gen-bounding-boxes)))))
      (unless *scene*
        (error "Failed to parse asset file '~A'" path))
      (unwind-protect
           (funcall callback)
        (%ai:release-import *scene*)))))


(defmacro with-imported-scene ((path) &body body)
  `(call-with-scene ,path (lambda () ,@body)))


(defclass scene ()
  ((meshes :initarg :meshes :initform nil :reader scene-meshes)
   (images :initarg :images :initform nil :reader scene-images)))


(defun destroy-scene (scene)
  (with-slots (meshes images) scene
    (loop for mesh in meshes
          do (destroy-mesh mesh))
    (loop for image in images
          do (destroy-image image))))


(defun parse-scene (path)
  (with-imported-scene (path)
    (let* ((*images* (list))
           (*materials* (loop with table = (make-hash-table :test #'equal)
                              for material in (parse-materials)
                              do (setf (gethash (material-id material) table) material)
                              finally (return table))))
      (make-instance 'scene
                     :meshes (parse-meshes)
                     :images (loop for image in *images*
                                   for full-path = (merge-pathnames
                                                    image
                                                    (uiop:pathname-directory-pathname path))
                                   collect (load-image image full-path))))))
