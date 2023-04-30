(cl:defpackage :%alien-works.tools.filament
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty)
                    (:u :alien-works.utils)
                    (:m :alien-works.math)
                    (:%aw.fm :%alien-works.filament))
  (:use :cl)
  (:export #:serialize-material
           #:material-data-pointer
           #:material-data-size
           #:with-serialized-material-data

           #:make-image
           #:image-width
           #:image-height
           #:image-channels
           #:image-data-ptr
           #:image-data-size
           #:destroy-image
           #:decode-image
           #:encode-image

           #:with-compressed-texture-encoder-builder
           #:compressed-texture-encoder-builder-linear
           #:compressed-texture-encoder-builder-mip-level
           #:encode-compressed-texture))


(cl:defpackage :alien-works.tools.graphics
  (:local-nicknames (:a :alexandria)
                    (:%aw.fm :%alien-works.filament)
                    (:%gx :%alien-works.graphics)
                    (:gx :alien-works.graphics)
                    (:%gxs :%alien-works.tools.filament)
                    (:u :alien-works.utils)
                    (:sv :static-vectors)
                    (:cref :cffi-c-ref)
                    (:m :alien-works.math)
                    (:mem :alien-works.memory)
                    (:%mem :%alien-works.memory)
                    (:host :alien-works.host))
  (:use :cl)
  (:import-from :%alien-works.tools.filament
                #:make-image
                #:image-width
                #:image-height
                #:image-channels
                #:destroy-image
                #:decode-image
                #:encode-image)
  (:export #:make-material
           #:serialize-material

           #:make-image
           #:image-width
           #:image-height
           #:image-channels
           #:destroy-image
           #:decode-image
           #:encode-image
           #:prepare-texture))
