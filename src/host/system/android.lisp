(cl:in-package :alien-works.host)

(u:define-enumval-extractor gl-attr %sdl:g-lattr)
(u:define-enumval-extractor gl-profile %sdl:g-lprofile)


(defun %init-host ()
  (%sdl:gl-set-attribute (gl-attr :context-profile-mask) (gl-profile :es))
  (%sdl:gl-set-attribute (gl-attr :context-major-version) 3)
  (%sdl:gl-set-attribute (gl-attr :context-minor-version) 2))


(defun %window-surface (wm-info)
  (cref:c-val ((wm-info %sdl:sys-w-minfo))
    (wm-info :info :android :window)))


(defun %native-gl-context (sdl-gl-context)
  sdl-gl-context)
