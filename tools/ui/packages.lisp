(cl:defpackage :%alien-works.tools.imgui
  (:use :cl)
  (:local-nicknames (:a :alexandria)
                    (:u :alien-works.utils)
                    (:math :alien-works.math)
                    (:host :alien-works.host)
                    (:%imgui :%filament.imgui)
                    (:cref :cffi-c-ref)
                    (:%host :%alien-works.host))
  (:export #:make-imgui-helper
           #:destroy-imgui-helper
           #:render-imgui
           #:update-font-atlas
           #:update-display-size
           #:define-ui-callback
           #:make-ui-callback
           #:destroy-ui-callback

           #:make-context
           #:destroy-context
           #:initialize-context
           #:with-bound-context
           #:update-mouse-position
           #:update-keyboard-buttons
           #:update-mouse-buttons
           #:update-mouse-wheel
           #:add-input-characters
           #:mouse-dragging-p
           #:mouse-drag-delta

           #:font-scale
           #:add-default-font
           #:add-font
           #:add-font-from-foreign
           #:with-font
           #:font-atlas-width
           #:desired-font-atlas-width

           #:framebuffer-scale

           #:with-style
           #:style
           #:style-window-rounding
           #:style-window-border
           #:update-touch-padding
           #:scale-style

           #:with-panel
           #:with-child-panel
           #:with-popup
           #:popup-open-p
           #:open-popup
           #:close-current-popup

           #:button
           #:checkbox
           #:text
           #:collapsing-header
           #:tree-node
           #:tree-pop
           #:with-tree-node
           #:selectable
           #:progress-bar
           #:same-line
           #:float-slider
           #:indent
           #:unindent
           #:with-indent
           #:item-active-p
           #:item-clicked-p
           #:with-combo

           #:text-input
           #:float-input
           #:color-input
           #:text-area

           #:with-menu-bar
           #:with-menu
           #:menu-item

           #:columns
           #:next-column

           #:focus-window
           #:focus-previous-item-by-default
           #:focus-keyboard))


(cl:defpackage :alien-works.tools.ui
  (:local-nicknames (:a :alexandria)
                    (:u :alien-works.utils)
                    (:math :alien-works.math)
                    (:host :alien-works.host)
                    (:%gx :%alien-works.graphics)
                    (:%fm :%alien-works.filament)
                    (:%ui :%alien-works.tools.imgui)
                    (:%mem :%alien-works.memory))
  (:use :cl)
  (:import-from :%alien-works.tools.imgui
                #:mouse-dragging-p
                #:mouse-drag-delta

                #:with-panel
                #:with-child-panel

                #:with-popup
                #:popup-open-p
                #:open-popup
                #:close-current-popup

                #:button
                #:checkbox
                #:text
                #:collapsing-header
                #:tree-node
                #:tree-pop
                #:with-tree-node
                #:selectable
                #:progress-bar
                #:same-line
                #:float-slider
                #:indent
                #:unindent
                #:with-indent
                #:item-active-p
                #:item-clicked-p
                #:with-combo

                #:float-input
                #:text-input
                #:color-input
                #:text-area

                #:with-style
                #:style-window-rounding
                #:style-window-border

                #:with-menu-bar
                #:with-menu
                #:menu-item

                #:focus-window
                #:focus-previous-item-by-default
                #:focus-keyboard

                #:with-font)
  (:export #:make-ui
           #:destroy-ui
           #:update-ui-input
           #:update-ui-size
           #:handle-ui-event
           #:render-ui
           #:ui

           #:mouse-dragging-p
           #:mouse-drag-delta

           #:with-panel
           #:with-child-panel

           #:with-popup
           #:popup-open-p
           #:open-popup
           #:close-current-popup

           #:button
           #:checkbox
           #:text
           #:collapsing-header
           #:tree-node
           #:tree-pop
           #:with-tree-node
           #:selectable
           #:progress-bar
           #:same-line
           #:float-slider
           #:indent
           #:unindent
           #:with-indent
           #:item-active-p
           #:item-clicked-p
           #:with-combo

           #:float-input
           #:text-input
           #:color-input
           #:text-area

           #:with-style
           #:style-window-rounding
           #:style-window-border

           #:with-menu-bar
           #:with-menu
           #:menu-item

           #:rows

           #:focus-window
           #:focus-previous-item-by-default
           #:focus-keyboard

           #:add-font
           #:use-memory-vector-font
           #:with-font

           #:skip-ui-processing))
