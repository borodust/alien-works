(cl:in-package :alien-works.host)


(defun %window-surface (wm-info)
  (cref:c-val ((wm-info %sdl:sys-w-minfo))
    (cffi:make-pointer (wm-info :info :android :surface))))
