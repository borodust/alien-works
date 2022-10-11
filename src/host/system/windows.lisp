(cl:in-package :alien-works.host)


(defun %init-host ()
  (%sdl:set-hint %sdl:+hint-windows-dpi-scaling+ "1"))


(defun %window-surface (wm-info)
  (cref:c-val ((wm-info %sdl:sys-w-minfo))
    (wm-info :info :win :window)))


(defun %native-gl-context (sdl-gl-context)
  sdl-gl-context)
