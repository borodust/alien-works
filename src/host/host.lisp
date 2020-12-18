(cl:in-package :alien-works.host)


(declaim (special *event*))

(defun call-with-window (callback)
  (%sdl:init %sdl:+init-video+)
  (let ((window (cffi:with-foreign-string (name "YO")
                  (%sdl:create-window name
                                      %sdl:+windowpos-undefined+
                                      %sdl:+windowpos-undefined+
                                      1280 960
                                      (cffi:foreign-enum-value '%sdl:window-flags :opengl)))))
    (when (cffi:null-pointer-p window)
      (error "Failed to create a window"))
    (unwind-protect
         (cref:c-with ((event %sdl:event))
           (let ((*event* (event &)))
             (funcall callback window)))
      (%sdl:destroy-window window)
      (%sdl:quit))))


(defmacro with-window ((window) &body body)
  `(call-with-window (lambda (,window) ,@body)))


(defun window-surface (window)
  (cref:c-with ((wm-info %sdl:sys-w-minfo))
    (setf (wm-info :version :major) %sdl:+major-version+
          (wm-info :version :minor) %sdl:+minor-version+
          (wm-info :version :patch) %sdl:+patchlevel+)

    (%sdl:get-window-wm-info window (wm-info &))

    (ecase (wm-info :subsystem)
      (:x11 (cffi:make-pointer (wm-info :info :x11 :window))))))


;;;
;;; EVENTS
;;;
(defun handle-events (handler)
  (loop for result = (%sdl:poll-event *event*)
        while (> result 0)
        do (funcall handler *event*)))


(defun event-type (event)
  (let* ((id (cref:c-ref event %sdl:event :type))
         (type (cffi:foreign-enum-keyword '%sdl:event-type id :errorp nil)))
    (if type type :uknown)))
