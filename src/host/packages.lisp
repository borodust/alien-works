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
                    (:%host :%alien-works.host)
                    (:%math :%alien-works.math))
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

           #:do-game-controller-ids
           #:grab-game-controller
           #:release-game-controller
           #:game-controller-name-by-id
           #:game-controller-power-level
           #:game-controller-button-count
           #:game-controller-button-pressed-p
           #:game-controller-axes-count
           #:game-controller-axis-short-value
           #:game-controller-axis-float-value
           #:game-controller-ball-count
           #:game-controller-ball-value
           #:game-controller-hat-count
           #:game-controller-hat-value

           #:load-gamepad-mappings-from-host-file
           #:do-gamepad-ids
           #:gamepad-name-by-id
           #:grab-gamepad
           #:release-gamepad
           #:gamepad-name
           #:gamepad-power-level
           #:gamepad-button-pressed-p
           #:gamepad-axis-short-value
           #:gamepad-axis-float-value

           #:run
           #:definit

           #:memcpy
           #:memset
           #:open-host-file
           #:with-open-host-file
           #:read-host-file-into-static-vector
           #:working-directory))
