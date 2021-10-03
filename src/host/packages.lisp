(cl:defpackage :%alien-works.host
  (:use)
  (:export #:get-clipboard-foreign-text
           #:set-clipboard-foreign-text

           #:event-input-foreign-text))


(cl:defpackage :alien-works.host
  (:local-nicknames (:cref :cffi-c-ref)
                    (:a :alexandria)
                    (:gray :trivial-gray-streams)
                    (:sv :static-vectors)
                    (:u :alien-works.utils)
                    (:%host :%alien-works.host))
  (:use :cl :%alien-works.host)
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
           #:event-key-scan-code
           #:event-mouse-button
           #:event-mouse-wheel

           #:make-mouse-state
           #:mouse-state
           #:mouse-state-x
           #:mouse-state-y
           #:mouse-state-left-button-pressed-p
           #:mouse-state-right-button-pressed-p
           #:mouse-state-middle-button-pressed-p


           #:make-keyboard-modifier-state
           #:keyboard-modifier-state
           #:keyboard-modifier-state-some-pressed-p

           #:scancode

           #:run
           #:definit

           #:memcpy
           #:memset
           #:open-host-file
           #:with-open-host-file
           #:read-host-file-into-static-vector
           #:working-directory))
