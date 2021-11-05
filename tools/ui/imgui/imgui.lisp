(cl:in-package :%alien-works.tools.imgui)


(u:define-enumval-extractor key-enum %imgui:im-gui-key-enum)


(defvar +undefined-float+ (- (float %imgui:+flt-max+ 0f0)))


;;;
;;; HELPER
;;;
(defun make-imgui-helper (filament-engine filament-view font-path)
  (iffi:with-intricate-instance (path %filament:utils+path
                                      'claw-utils:claw-string (namestring font-path))
    (iffi:make-intricate-instance '%filament:im-gui-helper
                                  '(:pointer %filament:engine) filament-engine
                                  '(:pointer %filament:view) filament-view
                                  '(:pointer %filament:utils+path) path
                                  '(:pointer %filament:im-gui-context) (cffi:null-pointer))))


(defun destroy-imgui-helper (helper)
  (iffi:destroy-intricate-instance '%filament:im-gui-helper helper))


(defun render-imgui (helper callback time-delta)
  (%filament:render
   '(claw-utils:claw-pointer %filament:im-gui-helper) helper
   ':float (float time-delta 0f0)
   '(claw-utils:claw-pointer %filament:im-gui-helper+callback) callback))


(defun update-display-size (imgui-helper width height scale-x scale-y)
  (%filament:set-display-size
   '(claw-utils:claw-pointer %filament:im-gui-helper) imgui-helper
   :int (floor width)
   :int (floor height)
   :float (float scale-x 0f0)
   :float (float scale-y 0f0)))


(defmacro define-ui-callback (name () &body body)
  (a:with-gensyms (engine view)
    `(iffi:deficallback ,name (,engine ,view) %filament:im-gui-helper+callback
       (declare (ignore ,engine ,view))
       ,@body
       (values))))


(defun make-ui-callback (callback-name)
  (iffi:make-intricate-callback callback-name))


(define-compiler-macro make-ui-callback (&whole whole callback-name)
  (if (and (listp callback-name) (eq (first callback-name) 'quote))
      `(iffi:make-intricate-callback ',(second callback-name))
      whole))


(defun destroy-ui-callback (name callback)
  (iffi:destroy-intricate-callback name callback))


(define-compiler-macro destroy-ui-callback (&whole whole callback-name callback)
  (if (and (listp callback-name) (eq (first callback-name) 'quote))
      `(iffi:destroy-intricate-callback ',(second callback-name) ,callback)
      whole))

;;;
;;; IMGUI
;;;
(defmacro with-vec2 ((vec &optional x y) &body body)
  (let ((vec-name (if (listp vec) (first vec) vec))
        (x-name (if (and x (listp x)) (first x) x))
        (y-name (if (and y (listp y)) (first y) y)))
    `(,@(if (listp vec)
            `(let ((,vec-name ,(second vec))) (declare (ignorable ,vec-name)))
            `(iffi:with-intricate-instance (,vec %imgui:im-vec2)))
      (iffi:with-intricate-slots %imgui:im-vec2 (,@(when x
                                                     `((,x-name %imgui:x)))
                                                 ,@(when y
                                                     `((,y-name %imgui:y))))
                                 ,vec-name
        ,@(when (and x (listp x))
            `((setf ,x-name ,(second x))))
        ,@(when (and y (listp y))
            `((setf ,y-name ,(second y))))
        ,@body))))


;;;
;;; IO
;;;

;; const char* (*GetClipboardTextFn)(void* user_data);
(cffi:defcallback get-clipboard-text (:pointer :char)
    ((user-data :pointer))
  (declare (ignore user-data))
  (%host:get-clipboard-foreign-text))


;; void (*SetClipboardTextFn)(void* user_data, const char* text);
(cffi:defcallback set-clipboard-text :void
    ((user-data :pointer)
     (text (:pointer :char)))
  (declare (ignore user-data))
  (%host:set-clipboard-foreign-text text)
  (values))


(defmacro with-io ((io) &body body)
  `(let ((,io (%imgui:get-io)))
     ,@body))


(defun init-io (io)
  (iffi:with-intricate-slots %imgui:im-gui-io
      ((key-map %imgui:key-map)
       (set-clipboard-fn %imgui:set-clipboard-text-fn)
       (get-clipboard-fn %imgui:get-clipboard-text-fn)
       (ini-filename %imgui:ini-filename)
       (log-filename %imgui:log-filename))
      io
    (cref:c-val ((key-map :int))
      (setf (key-map (key-enum :tab)) (host:scancode :tab)
            (key-map (key-enum :left-arrow)) (host:scancode :left)
            (key-map (key-enum :right-arrow)) (host:scancode :right)
            (key-map (key-enum :up-arrow)) (host:scancode :up)
            (key-map (key-enum :down-arrow)) (host:scancode :down)
            (key-map (key-enum :page-up)) (host:scancode :pageup)
            (key-map (key-enum :page-down)) (host:scancode :pagedown)
            (key-map (key-enum :home)) (host:scancode :home)
            (key-map (key-enum :end)) (host:scancode :end)
            (key-map (key-enum :insert)) (host:scancode :insert)
            (key-map (key-enum :delete)) (host:scancode :delete)
            (key-map (key-enum :backspace)) (host:scancode :backspace)
            (key-map (key-enum :space)) (host:scancode :space)
            (key-map (key-enum :enter)) (host:scancode :return)
            (key-map (key-enum :escape)) (host:scancode :escape)
            (key-map (key-enum :key-pad-enter)) (host:scancode :kp-enter)
            (key-map (key-enum :a)) (host:scancode :a)
            (key-map (key-enum :c)) (host:scancode :c)
            (key-map (key-enum :v)) (host:scancode :v)
            (key-map (key-enum :x)) (host:scancode :x)
            (key-map (key-enum :y)) (host:scancode :y)
            (key-map (key-enum :z)) (host:scancode :z)))
    (setf set-clipboard-fn (cffi:callback set-clipboard-text)
          get-clipboard-fn (cffi:callback get-clipboard-text)
          ini-filename (cffi:null-pointer)
          log-filename (cffi:null-pointer))))


(defun (setf font-scale) (scale io)
  (iffi:with-intricate-slots %imgui:im-gui-io ((value %imgui:font-global-scale)) io
    (setf value (float scale 0f0))))


(defun font-scale (io)
  (iffi:with-intricate-slots %imgui:im-gui-io ((value %imgui:font-global-scale)) io
    value))


(defun update-mouse-position (io x y)
  (iffi:with-intricate-slots %imgui:im-gui-io ((mouse-pos %imgui:mouse-pos)) io
    (with-vec2 ((pos mouse-pos) mouse-x mouse-y)
      (if (and x y)
          (setf mouse-x (float x 0f0)
                mouse-y (float y 0f0))
          (setf mouse-x +undefined-float+
                mouse-y +undefined-float+)))))


(defun update-keyboard-buttons (io scan-code pressed-p shift-p alt-p ctrl-p super-p)
  (iffi:with-intricate-slots %imgui:im-gui-io ((keys-down %imgui:keys-down)
                                                        (key-shift %imgui:key-shift)
                                                        (key-alt %imgui:key-alt)
                                                        (key-ctrl %imgui:key-ctrl)
                                                        (key-super %imgui:key-super))
                             io

    (assert (<= 0 scan-code 511)) ; see %imgui:keys-down array in ImGuiIO
    (cref:c-val ((keys-down :bool))
      (setf (keys-down scan-code) (and pressed-p t)))

    (setf key-shift (and shift-p t)
          key-alt (and alt-p t)
          key-ctrl (and ctrl-p t)
          key-super (and super-p t))))


(defun update-mouse-buttons (io left-button-pressed-p right-button-pressed-p middle-button-pressed-p)
  (iffi:with-intricate-slots %imgui:im-gui-io ((mouse-down %imgui:mouse-down)) io
    (cref:c-val ((mouse-down :bool))
      (setf (mouse-down 0) (and left-button-pressed-p t)
            (mouse-down 1) (and right-button-pressed-p t)
            (mouse-down 2) (and middle-button-pressed-p t)))))


(defun update-mouse-wheel (io y-offset &optional x-offset)
  (iffi:with-intricate-slots %imgui:im-gui-io ((mouse-wheel-x %imgui:mouse-wheel-h)
                                               (mouse-wheel-y %imgui:mouse-wheel))
                             io
    (incf mouse-wheel-x (float (or x-offset 0f0) 0f0))
    (incf mouse-wheel-y (float y-offset 0f0))))


(defun add-input-characters (io text)
  (%imgui:add-input-characters-utf8
   '(claw-utils:claw-pointer %imgui:im-gui-io) io
   'claw-utils:claw-string text))



(defun mouse-dragging-p (button &optional lock-threshold)
  (%imgui:is-mouse-dragging
   '%filament.imgui:im-gui-mouse-button (ecase button
                                           (:left 0)
                                           (:right 1)
                                           (:middle 2))
   :float (float (or lock-threshold -1f0) 0f0)))


(defun mouse-drag-delta (button result-vec2 &optional lock-threshold)
  (with-vec2 (vec x y)
    (%imgui:get-mouse-drag-delta
     '(claw-utils:claw-pointer %filament.imgui:im-vec2) vec
     '%filament.imgui:im-gui-mouse-button (ecase button
                                             (:left 0)
                                             (:right 1)
                                             (:middle 2))
     :float (float (or lock-threshold -1f0) 0f0))
    (setf (math:vec2 result-vec2 0) x
          (math:vec2 result-vec2 1) y)
    result-vec2))


;;;
;;; STYLE
;;;
(defmacro with-style ((style) &body body)
  `(let ((,style (%imgui:get-style)))
     ,@body))


(defun scale-style (style scale)
  (%imgui:scale-all-sizes
   '(claw-utils:claw-pointer %filament.imgui::im-gui-style) style
   :float (float scale 0f0)))


(defun update-touch-padding (style x-padding y-padding)
  (iffi:with-intricate-slots %imgui:im-gui-style ((value %imgui:touch-extra-padding)) style
    (with-vec2 ((padding value) x y)
      (setf x (float x-padding 0f0)
            y (float y-padding 0f0)))))

;;;
;;; WIDGETS
;;;
(defun show-demo-window ()
  (cref:c-with ((closed :bool))
    (setf closed t)
    (%imgui:show-demo-window '(claw-utils:claw-pointer :bool) (closed &))
    (not closed)))


(defmacro with-panel ((text &key on-close width height) &body body)
  (a:with-gensyms (keep-open close-not-clicked result size x y)
    (a:once-only (text)
      `(cref:c-with ((,close-not-clicked :bool))
         (setf ,close-not-clicked t)
         (,@(if (or width height)
                `(with-vec2 (,size ,x ,y)
                   (setf ,x (float ,(or width 0f0) 0f0)
                         ,y (float ,(or height 0f0) 0f0))
                   (%imgui:set-next-window-size
                    '(claw-utils:claw-pointer %filament.imgui:im-vec2) ,size
                    '%filament.imgui:im-gui-cond 0))
                `(progn))
          (let ((,keep-open (%imgui:begin
                             'claw-utils:claw-string ,text
                             '(claw-utils:claw-pointer :bool) (,close-not-clicked &)
                             '%imgui:im-gui-window-flags 0))
                ,@(when on-close
                    `(,result)))
            (unwind-protect
                 (when ,keep-open
                   ,@(if on-close
                         `((setf ,result (progn ,@body)))
                         body))
              (%imgui:end)
              ,(when on-close
                 `(unless ,close-not-clicked
                    (funcall ,on-close ,result))))))))))


(defun button (text)
  (with-vec2 (vec x y)
    (setf x (float 0 0f0)
          y (float 0 0f0))
    (%imgui:button
     'claw-utils:claw-string text
     '(claw-utils:claw-pointer %imgui:im-vec2) vec)))


(defun checkbox (text &optional checked)
  (cref:c-with ((fchecked :bool))
    (setf fchecked (and checked t))
    (%imgui:checkbox
     'claw-utils:claw-string text
     '(claw-utils:claw-pointer :bool) (fchecked &))))


(defun text (text &rest args)
  (%imgui:text-unformatted
   'claw-utils:claw-string (if args
                               (apply #'format nil text args)
                               text)
   'claw-utils:claw-string (cffi:null-pointer)))


(defun collapsing-header (text &key)
  (%imgui:collapsing-header
   'claw-utils:claw-string text
   '(claw-utils:claw-pointer :bool) (cffi:null-pointer)
   '%filament.imgui:im-gui-tree-node-flags 0))


(defun tree-node (text &key)
  (%imgui:tree-node-ex
   'claw-utils:claw-string text
   '%filament.imgui:im-gui-tree-node-flags 0))


(defun tree-pop ()
  (%imgui:tree-pop))


(defmacro with-tree-node ((text &rest keys &key) &body body)
  `(when (tree-node ,text ,@keys)
     (unwind-protect
          (progn ,@body)
       (tree-pop))))


(defun selectable (text &key selected)
  (with-vec2 (vec (x 0f0) (y 0f0))
    (%imgui:selectable
     'claw-utils:claw-string text
     ':bool selected
     '%filament.imgui:im-gui-selectable-flags 0
     '(claw-utils:claw-pointer %filament.imgui:im-vec2) vec)))


(defun progress-bar (progress-value &key overlay)
  (with-vec2 (vec (x -1f0) (y 0f0))
    (%imgui:progress-bar
     :float progress-value
     '(claw-utils:claw-pointer %filament.imgui:im-vec2) vec
     'claw-utils:claw-string overlay)))


(defun same-line (&key offset spacing)
  (%imgui:same-line :float (or offset 0f0) :float (or spacing -1f0)))


(defun float-slider (label value &key min max format power)
  (cref:c-with ((fvalue :float))
    (setf fvalue (float value 0f0))
    (let ((changed-p (%imgui:slider-float
                      'claw-utils:claw-string (or label "")
                      '(claw-utils:claw-pointer :float) (fvalue &)
                      :float (float (or min 0f0) 0f0)
                      :float (float (or max 1f0) 0f0)
                      'claw-utils:claw-string (or format "%.3f")
                      :float (float (or power 1f0) 0f0))))
      (values fvalue changed-p))))


(defun indent (&optional width)
  (%imgui:indent :float (float (or width 0f0) 0f0)))


(defun unindent (&optional width)
  (%imgui:unindent :float (float (or width 0f0) 0f0)))


(defun item-active-p ()
  (%imgui:is-item-active))


(defun float-input (text value &key step step-fast format)
  (cref:c-with ((fvalue :float))
    (setf fvalue (float value 0f0))
    (let ((changed-p (%imgui:input-float
                      'claw-utils:claw-string text
                      '(claw-utils:claw-pointer :float) (fvalue &)
                      :float (float (or step 1f0) 0f0)
                      :float (float (or step-fast 100f0) 0f0)
                      'claw-utils:claw-string (or format "%.3f")
                      '%filament.imgui:im-gui-input-text-flags 0)))
      (values fvalue changed-p))))
