(cl:in-package :alien-works.resources)


(defclass image ()
  ((name :initarg :name :initform (error ":name missing") :reader image-name)
   (data :initarg :data :initform (error ":data missing") :reader image-data)
   (width :initarg :width :initform (error ":width missing") :reader image-width)
   (height :initarg :height :initform (error ":height missing") :reader image-height)
   (channels :initarg :channels :initform (error ":channels missing") :reader image-channels)))


(defun load-image (name path)
  (cref:c-with ((width :int)
                (height :int)
                (channels :int))
    (cffi:with-foreign-string (path (namestring path))
      (let ((data (%stb.image:load path (width &) (height &) (channels &) 0)))
        (make-instance 'image
                       :name name
                       :data data
                       :width width
                       :height height
                       :channels channels)))))


(defun destroy-image (image)
  (with-slots (data) image
    (%stb.image:image-free data)))
