(cl:in-package :%alien-works.filament)


(defun create-box (min-x min-y min-z max-x max-y max-z)
  (let ((box (iffi:make-intricate-instance '%filament:box)))
    (with-vec3f (min-vec min-x min-y min-z)
      (with-vec3f (max-vec max-x max-y max-z)
        (%filament:set
         '(claw-utils:claw-pointer %filament::box) box
         '(claw-utils:claw-pointer %filament::math+float3) min-vec
         '(claw-utils:claw-pointer %filament::math+float3) max-vec)))
    box))


(defun destroy-box (box)
  (iffi:destroy-intricate-instance '%filament:box box))


(defmacro with-box ((box min-x min-y min-z max-x max-y max-z) &body body)
  `(let ((,box (create-box ,min-x ,min-y ,min-z ,max-x ,max-y ,max-z)))
     (unwind-protect
          (progn ,@body)
       (destroy-box ,box))))
