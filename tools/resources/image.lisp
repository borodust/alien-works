(cl:in-package :alien-works.tools.resources)


(defclass image ()
  ((name :initarg :name :initform (error ":name missing") :reader image-name)
   (data :initarg :data :initform (error ":data missing") :reader image-data)
   (width :initarg :width :initform (error ":width missing") :reader image-width)
   (height :initarg :height :initform (error ":height missing") :reader image-height)
   (channels :initarg :channels :initform (error ":channels missing") :reader image-channels)))


(defun load-image (name path &key (premultiply-alpha t))
  (cref:c-with ((width :int)
                (height :int)
                (channels :int))
    (cffi:with-foreign-string (path (namestring path))
      (%stb.image:set-unpremultiply-on-load 1)
      (let ((data (%stb.image:load path (width &) (height &) (channels &) 0)))
        (when (and (= channels 4) premultiply-alpha)
          (loop for pixel = data then (cffi:inc-pointer pixel channels)
                for idx from 0 below (* width height)
                do (cref:c-val ((pixel :uint8))
                     (let ((alpha (denormalize-uint8 (pixel 3))))
                       (setf (pixel 0) (floor (* (pixel 0) alpha))
                             (pixel 1) (floor (* (pixel 1) alpha))
                             (pixel 2) (floor (* (pixel 2) alpha)))))))
        (make-instance 'image
                       :name name
                       :data data
                       :width width
                       :height height
                       :channels channels)))))


(defun save-image (image path)
  (%stb.image.write:write-png (namestring path)
                              (image-width image)
                              (image-height image)
                              (image-channels image)
                              (image-data image)
                              0))


(defun destroy-image (image)
  (with-slots (data) image
    (%stb.image:image-free data)))


(defun images-to-cubemap-cross (px-path nx-path py-path ny-path pz-path nz-path
                                target-path)
  (let* ((images (loop for path in (list px-path nx-path py-path ny-path pz-path nz-path)
                       collect (load-image (file-namestring path) path)))
         (width (image-width (first images)))
         (height (image-height (first images)))
         (channels (image-channels (first images))))
    (loop for image in images
          unless (and (= (image-width image) width)
                      (= (image-height image) height)
                      (= (image-channels image) channels))
            do (error "Cubemap face image with wrong dimensions found"))
    (let* ((target-width (* width 4))
           (target-height (* height 3))
           (target-size (* channels target-width target-height))
           (target-data (cffi:foreign-alloc :char :count target-size)))
      (host:memset target-data 0 target-size)
      (flet ((%insert (image x-sector y-sector)
               (let ((x (* x-sector width))
                     (y (* y-sector height)))
                 (loop with dst-stride = (* target-width channels)
                       with src-stride = (* width channels)
                       with src-data = (image-data image)
                       for j below height
                       for dst-ptr = (cffi:inc-pointer target-data (+ (* x channels) (* y dst-stride)))
                         then (cffi:inc-pointer dst-ptr dst-stride)
                       for src-ptr = src-data then (cffi:inc-pointer src-ptr src-stride)
                       do (host:memcpy dst-ptr src-ptr src-stride)))))
        (destructuring-bind (px nx py ny pz nz) images
          (%insert px 2 1)
          (%insert nx 0 1)
          (%insert py 1 0)
          (%insert ny 1 2)
          (%insert pz 1 1)
          (%insert nz 3 1))
        (save-image (make-instance 'image
                                   :width target-width
                                   :height target-height
                                   :data target-data
                                   :channels channels
                                   :name (file-namestring target-path))
                    target-path)))))
