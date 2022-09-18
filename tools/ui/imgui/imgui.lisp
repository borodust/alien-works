(cl:in-package :%alien-works.tools.imgui)


(declaim (special *context*))


(u:define-enumval-extractor key-enum %imgui:im-gui-key-enum)
(u:define-enumval-extractor style-var-enum %imgui:im-gui-style-var-enum)
(u:define-enumval-extractor mouse-button-enum %imgui:im-gui-mouse-button-enum)

(u:define-enumbit-combiner window-flags-enum %imgui:im-gui-window-flags-enum)
(u:define-enumbit-combiner color-edit-flags-enum %imgui:im-gui-color-edit-flags-enum)
(u:define-enumbit-combiner tree-node-flags-enum %imgui:im-gui-tree-node-flags-enum)
(u:define-enumbit-combiner input-text-flags-enum %imgui:im-gui-input-text-flags-enum)

(defvar +undefined-float+ (- (float %imgui:+flt-max+ 0f0)))

;;;
;;; HELPER
;;;
(defun make-imgui-helper (context filament-engine filament-view font-path)
  (iffi:with-intricate-instance (path %filament:utils+path
                                      'claw-utils:claw-string (namestring font-path))
    (iffi:make-intricate-instance '%filament:im-gui-helper
                                  '(:pointer %filament:engine) filament-engine
                                  '(:pointer %filament:view) filament-view
                                  '(:pointer %filament:utils+path) path
                                  '(:pointer %filament:im-gui-context) context)))


(defun destroy-imgui-helper (helper)
  (iffi:destroy-intricate-instance '%filament:im-gui-helper helper))


(defun render-imgui (helper callback time-delta)
  (%filament:render
   '(claw-utils:claw-pointer %filament:im-gui-helper) helper
   ':float (float time-delta 0f0)
   '(claw-utils:claw-pointer %filament:im-gui-helper+callback) callback))


(defun update-font-atlas (helper engine)
  (%filament:create-atlas-texture
   '(claw-utils:claw-pointer %filament:im-gui-helper) helper
   '(claw-utils:claw-pointer %filament:engine) engine))


(defun update-display-size (imgui-helper width height scale-x scale-y)
  (%filament:set-display-size
   '(claw-utils:claw-pointer %filament:im-gui-helper) imgui-helper
   :int (floor width)
   :int (floor height)
   :float (float scale-x 0f0)
   :float (float scale-y 0f0)
   :bool nil))


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


(defun make-context ()
  (%filament.imgui:create-context
   '(claw-utils:claw-pointer %filament.imgui:im-font-atlas) nil))


(defun destroy-context (context)
  (%filament.imgui:destroy-context
   '(claw-utils:claw-pointer %filament.imgui:im-gui-context) context))


(defmacro with-bound-context ((context) &body body)
  (a:once-only (context)
    (a:with-gensyms (prev-context)
      `(let ((,prev-context (%imgui:get-current-context))
             (*context* ,context))
         (unwind-protect
              (progn
                (%imgui:set-current-context
                 '(claw-utils:claw-pointer %filament.imgui:im-gui-context) ,context)
                ,@body)
           (%imgui:set-current-context
            '(claw-utils:claw-pointer %filament.imgui:im-gui-context) ,prev-context))))))


(defun initialize-context ()
  (iffi:with-intricate-slots %imgui:im-gui-io
      ((key-map %imgui:key-map)
       (set-clipboard-fn %imgui:set-clipboard-text-fn)
       (get-clipboard-fn %imgui:get-clipboard-text-fn)
       (ini-filename %imgui:ini-filename)
       (log-filename %imgui:log-filename))
      (%imgui:get-io)
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


(defun (setf font-scale) (scale)
  (iffi:with-intricate-slots %imgui:im-gui-io ((value %imgui:font-global-scale)) (%imgui:get-io)
    (setf value (float scale 0f0))))


(defun font-scale ()
  (iffi:with-intricate-slots %imgui:im-gui-io ((value %imgui:font-global-scale)) (%imgui:get-io)
    value))


(defun framebuffer-scale ()
  (iffi:with-intricate-slots %imgui:im-gui-io ((value %imgui:display-framebuffer-scale)) (%imgui:get-io)
    (with-vec2 ((vec value) x y)
      (values x y))))


(defun set-framebuffer-scale (new-x &optional new-y)
  (iffi:with-intricate-slots %imgui:im-gui-io ((value %imgui:display-framebuffer-scale)) (%imgui:get-io)
    (with-vec2 ((vec value) x y)
      (setf x (float new-x 0f0)
            y (float (or new-y new-x) 0f0))
      (values x y))))


(defsetf framebuffer-scale () (x y)
  `(set-framebuffer-scale ,x ,@(when y `(,y))))


(defun update-mouse-position (x y)
  (iffi:with-intricate-slots %imgui:im-gui-io ((mouse-pos %imgui:mouse-pos)) (%imgui:get-io)
    (with-vec2 ((pos mouse-pos) mouse-x mouse-y)
      (if (and x y)
          (setf mouse-x (float x 0f0)
                mouse-y (float y 0f0))
          (setf mouse-x +undefined-float+
                mouse-y +undefined-float+)))))


(defun update-keyboard-buttons (scan-code pressed-p shift-p alt-p ctrl-p super-p)
  (iffi:with-intricate-slots %imgui:im-gui-io ((keys-down %imgui:keys-down)
                                               (key-shift %imgui:key-shift)
                                               (key-alt %imgui:key-alt)
                                               (key-ctrl %imgui:key-ctrl)
                                               (key-super %imgui:key-super))
                             (%imgui:get-io)
    (assert (<= 0 scan-code 511))       ; see %imgui:keys-down array in ImGuiIO
    (cref:c-val ((keys-down :bool))
      (setf (keys-down scan-code) (and pressed-p t)))

    (setf key-shift (and shift-p t)
          key-alt (and alt-p t)
          key-ctrl (and ctrl-p t)
          key-super (and super-p t))))


(defun update-mouse-buttons (left-button-pressed-p right-button-pressed-p middle-button-pressed-p)
  (iffi:with-intricate-slots %imgui:im-gui-io ((mouse-down %imgui:mouse-down)) (%imgui:get-io)
    (cref:c-val ((mouse-down :bool))
      (setf (mouse-down 0) (and left-button-pressed-p t)
            (mouse-down 1) (and right-button-pressed-p t)
            (mouse-down 2) (and middle-button-pressed-p t)))))


(defun update-mouse-wheel (y-offset &optional x-offset)
  (iffi:with-intricate-slots %imgui:im-gui-io ((mouse-wheel-x %imgui:mouse-wheel-h)
                                               (mouse-wheel-y %imgui:mouse-wheel))
                             (%imgui:get-io)
    (incf mouse-wheel-x (float (or x-offset 0f0) 0f0))
    (incf mouse-wheel-y (float y-offset 0f0))))


(defun add-input-characters (text)
  (%imgui:add-input-characters-utf8
   '(claw-utils:claw-pointer %imgui:im-gui-io) (%imgui:get-io)
   'claw-utils:claw-string text))


(defun mouse-button-code (keyword)
  (ecase keyword
    (:left (mouse-button-enum :left))
    (:right (mouse-button-enum :right))
    (:middle (mouse-button-enum :middle))))


(defun mouse-dragging-p (button &optional lock-threshold)
  (%imgui:is-mouse-dragging
   '%filament.imgui:im-gui-mouse-button (mouse-button-code button)
   :float (float (or lock-threshold -1f0) 0f0)))


(defun mouse-drag-delta (button result-vec2 &optional lock-threshold)
  (with-vec2 (vec x y)
    (%imgui:get-mouse-drag-delta
     '(claw-utils:claw-pointer %filament.imgui:im-vec2) vec
     '%filament.imgui:im-gui-mouse-button (mouse-button-code button)
     :float (float (or lock-threshold -1f0) 0f0))
    (setf (math:vec2 result-vec2 0) x
          (math:vec2 result-vec2 1) y)
    result-vec2))


(defun add-default-font (&key pixel-size)
  (iffi:with-intricate-slots %imgui:im-gui-io ((fonts %imgui:fonts)) (%imgui:get-io)
    (iffi:with-intricate-instance (font-config %imgui:im-font-config)
      (iffi:with-intricate-slots %imgui:im-font-config ((%size-pixels %imgui:size-pixels)) font-config
        (when (numberp pixel-size)
          (setf %size-pixels (float pixel-size 0f0))))
      (%imgui:add-font-default
       '(claw-utils:claw-pointer %filament.imgui:im-font-atlas) fonts
       '(claw-utils:claw-pointer %filament.imgui:im-font-config) font-config))))


(defun add-font-from-foreign (foreign-data-ptr foreign-data-size pixel-size
                              &key transfer-ownership)
  (iffi:with-intricate-slots %imgui:im-gui-io ((fonts %imgui:fonts)) (%imgui:get-io)
    (iffi:with-intricate-instance (font-config %imgui:im-font-config)
      (iffi:with-intricate-slots %imgui:im-font-config ((owned-by-atlas %imgui:font-data-owned-by-atlas)
                                                        (oversample-h %imgui:oversample-h)
                                                        (oversample-v %imgui:oversample-v)
                                                        (pixel-snap-h %imgui:pixel-snap-h))
                                 font-config
        (setf owned-by-atlas transfer-ownership
              oversample-h 1
              oversample-v 1
              pixel-snap-h t)
        (%imgui:add-font-from-memory-ttf
         '(claw-utils:claw-pointer %filament.imgui:im-font-atlas) fonts
         '(claw-utils:claw-pointer :void) foreign-data-ptr
         :int foreign-data-size
         :float (float pixel-size 0f0)
         '(claw-utils:claw-pointer %filament.imgui:im-font-config) font-config
         '(claw-utils:claw-pointer %filament.imgui:im-wchar) (cffi:null-pointer))))))


(defun add-font (data pixel-size)
  (let* ((foreign-data-size (length data))
         (foreign-data-ptr (cffi:foreign-array-alloc
                            data `(:array :uint8 ,foreign-data-size))))
    (add-font-from-foreign foreign-data-ptr foreign-data-size pixel-size :transfer-ownership t)))


(defmacro with-font ((font) &body body)
  `(unwind-protect
        (progn
          (%imgui:push-font '(claw-utils:claw-pointer %filament.imgui:im-font) ,font)
          ,@body)
     (%imgui:pop-font)))

;;;
;;; STYLE
;;;
(defmacro with-style ((&key window-rounding window-border) &body body)
  (let ((style-var-count 0))
    (flet ((%push-float-var (value name)
             (when value
               (incf style-var-count)
               `((%imgui:push-style-var
                  '%filament.imgui:im-gui-style-var ,(style-var-enum name)
                  :float (float ,value 0f0))))))
      `(progn
         ,@(%push-float-var window-rounding :window-rounding)
         ,@(%push-float-var window-border :window-border-size)
         (unwind-protect
              (progn ,@body)
           ,@(unless (zerop style-var-count)
               `((%imgui:pop-style-var :int ,style-var-count))))))))


(defun style ()
  (%imgui:get-style))


(defun style-window-rounding ()
  (iffi:intricate-slot-value (style) '%imgui:im-gui-style '%imgui:window-rounding))


(defun style-window-border ()
  (iffi:intricate-slot-value (style) '%imgui:im-gui-style '%imgui:window-border-size))


(defun scale-style (style scale)
  (%imgui:scale-all-sizes
   '(claw-utils:claw-pointer %filament.imgui:im-gui-style) style
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

(defmacro with-panel ((text &key on-close
                              x y
                              width height
                              menu-bar
                              (background t)
                              (resizable t)
                              (movable t)
                              (title-bar t)
                              (vertically-scrollable t)
                              horizontally-scrollable
                              (collapsible t)
                              (focus-on-appearing t)
                              (bring-to-front-on-focus t)
                              auto-resize)
                      &body body)
  (a:with-gensyms (keep-open close-not-clicked size pos zero-vec tmp-x tmp-y)
    (a:once-only (text)
      `(cref:c-with ((,close-not-clicked :bool))
         (setf ,close-not-clicked t)
         ,@(when (or width height)
             `((with-vec2 (,size ,tmp-x ,tmp-y)
                 (setf ,tmp-x (float ,(or width 0f0) 0f0)
                       ,tmp-y (float ,(or height 0f0) 0f0))
                 (%imgui:set-next-window-size
                  '(claw-utils:claw-pointer %filament.imgui:im-vec2) ,size
                  '%filament.imgui:im-gui-cond 0))))
         ,@(when (or x y)
             `((with-vec2 (,pos ,tmp-x ,tmp-y)
                 (setf ,tmp-x (float ,(or x 0f0) 0f0)
                       ,tmp-y (float ,(or y 0f0) 0f0))
                 (with-vec2 (,zero-vec)
                   (%imgui:set-next-window-pos
                    '(claw-utils:claw-pointer %filament.imgui:im-vec2) ,pos
                    '%filament.imgui:im-gui-cond 0
                    '(claw-utils:claw-pointer %filament.imgui:im-vec2) ,zero-vec)))))
         (let ((,keep-open (%imgui:begin
                            'claw-utils:claw-string (string ,text)
                            '(claw-utils:claw-pointer :bool) (,close-not-clicked &)
                            '%imgui:im-gui-window-flags (window-flags-enum
                                                         ,@(when menu-bar
                                                             '(:menu-bar))
                                                         ,@(unless background
                                                             '(:no-background))
                                                         ,@(unless resizable
                                                             '(:no-resize))
                                                         ,@(unless movable
                                                             '(:no-move))
                                                         ,@(unless title-bar
                                                             '(:no-title-bar))
                                                         ,@(unless vertically-scrollable
                                                             '(:no-scrollbar))
                                                         ,@(when horizontally-scrollable
                                                             '(:horizontal-scrollbar))
                                                         ,@(unless collapsible
                                                             '(:no-collapse))
                                                         ,@(unless focus-on-appearing
                                                             '(:no-focus-on-appearing))
                                                         ,@(unless bring-to-front-on-focus
                                                             '(:no-bring-to-front-on-focus))
                                                         ,@(when auto-resize
                                                             '(:always-auto-resize))))))
           (unwind-protect
                (when ,keep-open
                  ,@body)
             (%imgui:end)
             ,@(when on-close
                 `((unless ,close-not-clicked
                     (funcall ,on-close))))))))))


(defmacro with-menu-bar (() &body body)
  `(when (%imgui:begin-menu-bar)
     (unwind-protect
          (progn ,@body)
       (%imgui:end-menu-bar))))


(defmacro with-menu ((title &optional (enabled t)) &body body)
  `(when (%imgui:begin-menu
          'claw-utils:claw-string (string ,title)
          :bool ,enabled)
     (unwind-protect
          (progn ,@body)
       (%imgui:end-menu))))


(defun menu-item (label)
  (%imgui:menu-item 'claw-utils:claw-string (string label)
                    'claw-utils:claw-string (cffi:null-pointer)
                    :bool nil
                    :bool t))


(defun button (text)
  (with-vec2 (vec x y)
    (setf x (float 0 0f0)
          y (float 0 0f0))
    (%imgui:button
     'claw-utils:claw-string (string text)
     '(claw-utils:claw-pointer %imgui:im-vec2) vec)))


(defun checkbox (text &optional checked)
  (cref:c-with ((fchecked :bool))
    (setf fchecked (and checked t))
    (%imgui:checkbox
     'claw-utils:claw-string (string text)
     '(claw-utils:claw-pointer :bool) (fchecked &))))


(defun text (text &rest args)
  (%imgui:text-unformatted
   'claw-utils:claw-string (if args
                               (apply #'format nil text args)
                               (string text))
   'claw-utils:claw-string (cffi:null-pointer)))


(defun collapsing-header (text &key open)
  (%imgui:collapsing-header
   'claw-utils:claw-string (string text)
   '(claw-utils:claw-pointer :bool) (cffi:null-pointer)
   '%filament.imgui:im-gui-tree-node-flags (apply #'tree-node-flags-enum
                                                  (append
                                                   (when open
                                                     '(:default-open))))))


(defun tree-node (text &key open)
  (%imgui:tree-node-ex
   'claw-utils:claw-string (string text)
   '%filament.imgui:im-gui-tree-node-flags (apply #'tree-node-flags-enum
                                                  (append
                                                   (when open
                                                     '(:default-open))))))


(defun tree-pop ()
  (%imgui:tree-pop))


(defmacro with-tree-node ((text &rest keys &key &allow-other-keys) &body body)
  `(when (tree-node ,text ,@keys)
     (unwind-protect
          (progn ,@body)
       (tree-pop))))


(defun selectable (text &key selected)
  (with-vec2 (vec (x 0f0) (y 0f0))
    (%imgui:selectable
     'claw-utils:claw-string (string text)
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
  (%imgui:same-line :float (float (or offset 0f0) 0f0)
                    :float (float (or spacing -1f0) 0f0)))


(defun float-slider (label value &key min max format)
  (cref:c-with ((fvalue :float))
    (setf fvalue (float value 0f0))
    (let ((changed-p (%imgui:slider-float
                      'claw-utils:claw-string (string (or label ""))
                      '(claw-utils:claw-pointer :float) (fvalue &)
                      :float (float (or min 0f0) 0f0)
                      :float (float (or max 1f0) 0f0)
                      'claw-utils:claw-string (or format "%.3f")
                      '%filament.imgui:im-gui-slider-flags 0)))
      (values fvalue changed-p))))


(defun indent (&optional width)
  (%imgui:indent :float (float (or width 0f0) 0f0)))


(defun unindent (&optional width)
  (%imgui:unindent :float (float (or width 0f0) 0f0)))


(defmacro with-indent ((&optional width) &body body)
  (a:once-only (width)
    `(unwind-protect
          (progn
            (indent ,width)
            ,@body)
       (unindent ,width))))


(defun item-active-p ()
  (%imgui:is-item-active))


(defun float-input (text value &key step step-fast format)
  (cref:c-with ((fvalue :float))
    (setf fvalue (float value 0f0))
    (let ((changed-p (%imgui:input-float
                      'claw-utils:claw-string (string text)
                      '(claw-utils:claw-pointer :float) (fvalue &)
                      :float (float (or step 1f0) 0f0)
                      :float (float (or step-fast 100f0) 0f0)
                      'claw-utils:claw-string (or format "%.3f")
                      '%filament.imgui:im-gui-input-text-flags 0)))
      (values fvalue changed-p))))


(defun text-input (label size &key text)
  (let ((completed nil))
    (values
     (cffi:with-foreign-pointer-as-string (str-ptr size)
       (if text
           (cffi:lisp-string-to-foreign text str-ptr size)
           (setf (cffi:mem-ref str-ptr :char 0) 0))
       (setf
        completed
        (%imgui:input-text 'claw-utils:claw-string (string label)
                           'claw-utils:claw-string str-ptr
                           '%filament.imgui:size-t size
                           '%filament.imgui:im-gui-input-text-flags (input-text-flags-enum
                                                                     :enter-returns-true)
                           '%filament.imgui:im-gui-input-text-callback (cffi:null-pointer)
                           '(claw-utils:claw-pointer :void) (cffi:null-pointer))))
     completed)))


(defun next-item-width (value)
  (%imgui:set-next-item-width :float (float value 0f0)))


(defun spacing ()
  (%imgui:spacing))


(defun open-popup (id)
  (%imgui:open-popup
   'claw-utils:claw-string (string id)
   '%filament.imgui:im-gui-popup-flags 0))


(defun close-current-popup ()
  (%imgui:close-current-popup))


(defun popup-open-p (id)
  (%imgui:is-popup-open
   'claw-utils:claw-string (string id)
   '%filament.imgui:im-gui-popup-flags 0))


(defmacro with-popup ((id &key modal) &body body)
  (let ((body `((unwind-protect
                     (progn ,@body)
                  (%imgui:end-popup)))))
    (if modal
        (a:with-gensyms (close-not-clicked)
          `(cref:c-with ((,close-not-clicked :bool))
             (setf ,close-not-clicked t)
             (when (%imgui:begin-popup-modal 'claw-utils:claw-string (string ,id)
                                             '(claw-utils:claw-pointer :bool) (,close-not-clicked &)
                                             '%filament.imgui:im-gui-window-flags 0)
               ,@body)))
        `(when (%imgui:begin-popup 'claw-utils:claw-string (string ,id)
                                   '%filament.imgui:im-gui-window-flags 0)
           ,@body))))


(defmacro with-child-panel ((id &key x y width height borderless) &body body)
  (a:with-gensyms (size pos zero-vec tmp-x tmp-y)
    `(unwind-protect
          (progn
            ,@(when (or x y)
                `((with-vec2 (,pos ,tmp-x ,tmp-y)
                    (setf ,tmp-x (float ,(or x 0f0) 0f0)
                          ,tmp-y (float ,(or y 0f0) 0f0))
                    (with-vec2 (,zero-vec)
                      (%imgui:set-next-window-pos
                       '(claw-utils:claw-pointer %filament.imgui:im-vec2) ,pos
                       '%filament.imgui:im-gui-cond 0
                       '(claw-utils:claw-pointer %filament.imgui:im-vec2) ,zero-vec)))))
            (with-vec2 (,size ,tmp-x ,tmp-y)
              (setf ,tmp-x (float ,(or width 0f0) 0f0)
                    ,tmp-y (float ,(or height 0f0) 0f0))
              (%imgui:begin-child
               'claw-utils:claw-string (string ,id)
               '(claw-utils:claw-pointer %filament.imgui:im-vec2) ,size
               :bool ,(not borderless)
               '%filament.imgui:im-gui-window-flags 0))
            ,@body)
       (%imgui:end-child))))


(defmacro with-combo ((label &key text) &body body)
  `(when (%imgui:begin-combo
          'claw-utils:claw-string (string ,label)
          'claw-utils:claw-string (string (or ,text ""))
          '%filament.imgui:im-gui-combo-flags 0)
     (unwind-protect
          (progn ,@body)
       (%imgui:end-combo))))


(defun columns (count)
  (%imgui:columns
   :int (ceiling count)
   'claw-utils:claw-string (cffi:null-pointer)
   :bool nil))


(defun next-column ()
  (%imgui:next-column))


(defun color-input (label &optional (r 0f0) (g 0f0) (b 0f0) (a 1f0))
  (cref:c-with ((color :float :count 4))
    (setf (color 0) (float r 0f0)
          (color 1) (float g 0f0)
          (color 2) (float b 0f0)
          (color 3) (float a 0f0))
    (when (%imgui:color-edit4
           'claw-utils:claw-string (string label)
           '(:pointer :float) (color &)
           '%filament.imgui:im-gui-color-edit-flags (color-edit-flags-enum
                                                      :float
                                                      :alpha-bar
                                                      :alpha-preview-half))
      (values (color 0) (color 1) (color 2) (color 3)))))


(defun focus-window (&optional name)
  (if name
      (%imgui:set-window-focus 'claw-utils:claw-string (string name))
      (%imgui:set-window-focus)))


(defun focus-previous-item-by-default ()
  (%imgui:set-item-default-focus))


(defun focus-keyboard (&optional (offset 0))
  (%imgui:set-keyboard-focus-here :int (floor offset)))


(defun item-clicked-p (&optional (button :left))
  (%imgui:is-item-clicked
   '%filament.imgui:im-gui-mouse-button (mouse-button-code button)))
