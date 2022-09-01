(cl:in-package :alien-works.tools.resources)


(defun load-image (path)
  (%awt.fm:decode-image (alexandria:read-file-into-byte-vector path)))


(defun load-image-from-octet-vector (data)
  (%awt.fm:decode-image data))


(defun save-image (image path)
  (host:with-open-host-file (out path :direction :output)
    (%host:write-foreign-array (%awt.fm:image-data-ptr image)
                               (%awt.fm:image-data-size image)
                               out)))


(defun read-image-into-octet-vector (image)
  (let* ((data-ptr (%awt.fm:image-data-ptr image))
         (data-size (%awt.fm:image-data-size image))
         (result (make-array data-size :element-type '(unsigned-byte 8))))
    (u:with-pinned-array-pointer (result-ptr result)
      (alien-works:memcpy result-ptr data-ptr data-size))
    result))


(defun encode-image-octet-vector-into-png (data width height channels)
  (let ((image (%awt.fm:make-image width height channels)))
    (unwind-protect
         (alien-works:with-memory-vector (tmp
                                          (%awt.fm:image-data-size image))
           (u:with-pinned-array-pointer (data-ptr data)
             (host:memcpy (%awt.fm:image-data-ptr image)
                          data-ptr
                          (%awt.fm:image-data-size image)))
           (let ((written (%awt.fm:encode-image image tmp :format :png)))
             (make-array written :element-type '(unsigned-byte 8)
                                 :initial-contents tmp))))))


(defun images-to-cubemap-cross (px-path nx-path py-path ny-path pz-path nz-path
                                target-path)
  (let* ((images (loop for path in (list px-path nx-path py-path ny-path pz-path nz-path)
                       collect (load-image path)))
         (width (%awt.fm:image-width (first images)))
         (height (%awt.fm:image-height (first images)))
         (channels (%awt.fm:image-channels (first images))))
    (loop for image in images
          unless (and (= (%awt.fm:image-width image) width)
                      (= (%awt.fm:image-height image) height)
                      (= (%awt.fm:image-channels image) channels))
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
                       with src-data = (%awt.fm:image-data-ptr image)
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
        (not
         (zerop
          (save-image (make-instance 'image
                                     :width target-width
                                     :height target-height
                                     :data target-data
                                     :channels channels
                                     :name (file-namestring target-path))
                      target-path)))))))
