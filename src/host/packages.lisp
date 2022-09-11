(cl:defpackage :%alien-works.host
  (:use)
  (:export #:with-window

           #:get-clipboard-foreign-text
           #:set-clipboard-foreign-text

           #:event-input-foreign-text

           #:make-shared-context-thread
           #:window-graphics-context
           #:window-surface

           #:write-foreign-array))


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
           #:display-dpi
           #:list-displays

           #:window-display
           #:window-width
           #:window-height
           #:framebuffer-width
           #:framebuffer-height

           #:handle-events
           #:event-kind
           #:event-key-scan-code
           #:event-mouse-button
           #:event-mouse-wheel
           #:event-mouse-position
           #:event-game-controller-id
           #:event-game-controller-button
           #:event-gamepad-id
           #:event-gamepad-button
           #:event-text-input

           #:make-mouse-state
           #:mouse-state
           #:mouse-state-x
           #:mouse-state-y
           #:mouse-state-left-button-pressed-p
           #:mouse-state-right-button-pressed-p
           #:mouse-state-middle-button-pressed-p

           #:make-keyboard-modifier-state
           #:keyboard-modifier-state
           #:keyboard-modifier-state-pressed-p
           #:keyboard-modifier-state-some-pressed-p

           #:event-finger-id
           #:event-finger-x
           #:event-finger-y
           #:event-finger-x-offset
           #:event-finger-y-offset
           #:event-finger-position

           #:event-simple-gesture-finger-count
           #:event-simple-gesture-distance-offset
           #:event-simple-gesture-rotation-offset
           #:event-simple-gesture-x
           #:event-simple-gesture-y

           #:scancode

           #:do-game-controller-ids
           #:grab-game-controller
           #:release-game-controller
           #:game-controller-name-by-id
           #:game-controller-name
           #:game-controller-power-level
           #:game-controller-haptic-p
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
           #:gamepad-haptic-p
           #:gamepad-button-pressed-p
           #:gamepad-axis-short-value
           #:gamepad-axis-float-value

           #:do-haptic-device-ids
           #:grab-haptic-device
           #:grab-game-controller-haptic-device
           #:grab-gamepad-haptic-device
           #:release-haptic-device
           #:add-rumble
           #:play-rumble

           #:run
           #:definit

           #:memcpy
           #:memset
           #:open-host-file
           #:with-open-host-file
           #:read-host-file-into-static-vector
           #:read-host-file-into-shareable-vector
           #:working-directory))
