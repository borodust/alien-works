(cl:in-package :alien-works.host)

(u:define-enumval-extractor gl-attr %sdl:g-lattr)
(u:define-enumval-extractor gl-profile %sdl:g-lprofile)


(defun %init-host ())


(defun %window-surface (wm-info)
  (cref:c-val ((wm-info %sdl:sys-w-minfo))
    (wm-info :info :android :surface)))


(defun %native-gl-context (sdl-gl-context)
  sdl-gl-context)
