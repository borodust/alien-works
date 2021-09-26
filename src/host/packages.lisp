(cl:defpackage :alien-works.host
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:gray :trivial-gray-streams)
                    (:sv :static-vectors)
                    (:u :alien-works.utils))
  (:use :cl)
  (:export #:display-name
           #:display-x
           #:display-y
           #:display-width
           #:display-height
           #:display-orientation
           #:list-displays

           #:with-window
           #:window-display
           #:window-surface
           #:window-width
           #:window-height
           #:framebuffer-width
           #:framebuffer-height

           #:make-shared-context-thread

           #:handle-events
           #:event-type

           #:run
           #:definit

           #:memcpy
           #:memset
           #:open-host-file
           #:with-open-host-file
           #:read-host-file-into-static-vector
           #:working-directory))
