(cl:in-package :alien-works.host)


(defun %init-host ())


(defun %window-surface (wm-info)
  (cref:c-val ((wm-info %sdl:sys-w-minfo))
    (cffi:make-pointer (wm-info :info :x11 :window))))


(defun %native-gl-context (sdl-gl-context)
  sdl-gl-context)
