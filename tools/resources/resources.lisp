(cl:in-package :alien-works.tools.resources)


(defun resize-file (path new-size)
  (unless (zerop (%filament.util:aw+filament+tools+util+resize-file
                  'claw-utils:claw-string (namestring (truename path))
                  :unsigned-long new-size))
    (error "Failed to resize file ~A" path)))
