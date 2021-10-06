(cl:defpackage :%alien-works.tools.imgui
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:u :alien-works.utils)
                    (:host :alien-works.host)
                    (:%imgui :%filament.imgui)
                    (:cref :cffi-c-ref)
                    (:%host :%alien-works.host))
  (:export #:make-imgui-helper
           #:destroy-imgui-helper
           #:render-imgui
           #:update-display-size
           #:define-ui-callback
           #:make-ui-callback
           #:destroy-ui-callback

           #:with-io
           #:init-io
           #:update-mouse-position
           #:update-keyboard-buttons
           #:update-mouse-buttons
           #:update-mouse-wheel
           #:add-input-characters

           #:with-panel

           #:button
           #:checkbox
           #:label))


(cl:defpackage :alien-works.tools.ui
  (:local-nicknames (:a :alexandria)
                    (:u :alien-works.utils)
                    (:host :alien-works.host)
                    (:%gx :%alien-works.graphics)
                    (:%fm :%alien-works.filament)
                    (:%ui :%alien-works.tools.imgui))
  (:use :cl)
  (:import-from :%alien-works.tools.imgui
                #:with-panel
                #:button
                #:checkbox
                #:label)
  (:export #:make-ui
           #:destroy-ui
           #:update-ui-input
           #:handle-ui-event
           #:render-ui
           #:ui

           #:with-panel
           #:button
           #:checkbox
           #:label))
