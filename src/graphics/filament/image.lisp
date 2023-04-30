(cl:in-package :%alien-works.filament)


(defmacro with-compressed-texture-decoder ((decoder-var engine internal-format &rest internal-formats)
                                          &body body)
  (a:once-only (engine)
    `(iffi:with-intricate-instance
         (,decoder-var %filament:ktxreader+ktx2reader
                       '(claw-utils:claw-pointer %filament::ktxreader+ktx2reader+engine) ,engine
                       :bool nil)
       (%filament:ktxreader+request-format
        '(claw-utils:claw-pointer %filament::ktxreader+ktx2reader) ,decoder-var
        '%filament::texture+internal-format ,internal-format)
       ,@(loop :for format :in internal-formats
               :collect `(%filament:ktxreader+request-format
                          '(claw-utils:claw-pointer %filament::ktxreader+ktx2reader) ,decoder-var
                          '%filament::texture+internal-format ,format))
       ,@body)))


(defun decode-compressed-texture (decoder byte-vector &optional (transfer-function :linear))
  (u:with-pinned-array-pointer (data-ptr byte-vector)
    (%filament:ktxreader+load
     '(claw-utils:claw-pointer %filament::ktxreader+ktx2reader) decoder
     '(claw-utils:claw-pointer :void) data-ptr
     '%filament::size-t (length byte-vector)
     '%filament::ktxreader+ktx2reader+transfer-function transfer-function)))
