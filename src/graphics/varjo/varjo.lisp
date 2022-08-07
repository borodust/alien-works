(cl:in-package :%alien-works.varjo)


(declaim (special *root-function*))


(defun gen-shader-string (post-proc-obj)
  (let* ((funcs (varjo.internals::all-functions post-proc-obj))
         (func-code (remove nil (mapcar #'varjo.internals::glsl-code funcs)))
         (func-sigs (remove nil (a:mappend #'varjo.internals::signatures funcs))))
    (with-slots (env) post-proc-obj
      (format
       nil "~{~%~{~a~%~}~}"
       (remove nil
               (list (varjo.internals::used-user-structs post-proc-obj)
                     (varjo.internals::gs-invocations post-proc-obj)
                     (varjo.internals::shared-decls post-proc-obj)
                     (varjo.internals::remove-empty
                      (append
                       (mapcar #'varjo.internals::%glsl-decl
                               (varjo.internals::uniforms post-proc-obj))
                       (mapcar #'varjo.internals::%glsl-decl
                               (varjo.internals::stemcells post-proc-obj))))
                     func-sigs
                     (reverse func-code)))))))


(defun final-string-compose (post-proc-obj)
  (values (gen-shader-string post-proc-obj)
          post-proc-obj))

(defgeneric compile-pass (stage env))

(defun make-entry-function (entry-name env)
  (let ((func (first
               (varjo.internals::get-external-function-by-name *root-function* nil))))
    (varjo.internals:build-function entry-name
                                    (varjo.internals::in-args func)
                                    (varjo.internals::code func)
                                    nil env)))

(defmethod compile-pass ((stage varjo.internals::vertex-stage) env)
  (values (make-entry-function :|materialVertex| env)
          stage
          env))


(defmethod compile-pass ((stage varjo.internals::fragment-stage) env)
  (values (make-entry-function :|material| env)
          stage
          env))


(defun format-glsl (root-function stage-kind)
  (varjo.internals::flow-id-scope
    (let* ((stage (varjo:make-stage stage-kind '() nil '(:330) `((return))))
           (env (varjo.internals::%make-base-environment stage))
           (*root-function* root-function))

      (varjo.utils::pipe-> (stage env)
        #'varjo.internals::add-context-glsl-vars
        #'varjo.internals::add-context-glsl-funcs
        #'varjo.internals::validate-inputs
        #'varjo.internals::process-primitive-type
        #'varjo.internals::expand-input-variables
        #'varjo.internals::process-uniforms
        #'varjo.internals::process-shared
        #'compile-pass
        #'varjo.internals::make-post-process-obj
        #'varjo.internals::process-gs-invocations
        #'varjo.internals::process-output-primitive
        #'varjo.internals::make-out-set
        #'varjo.internals::check-stemcells
        #'varjo.internals::filter-used-items
        #'varjo.internals::validate-outputs
        #'varjo.internals::gen-in-arg-strings
        #'varjo.internals::gen-in-decl-strings
        #'varjo.internals::gen-out-var-strings
        #'varjo.internals::final-uniform-strings
        #'varjo.internals::gen-shared-decls
        #'varjo.internals::dedup-used-types
        #'final-string-compose))))


(varjo.internals::define-vari-special-operator letvar (bindings &rest body)
  :args-valid t
  :return
  (let* ((binding-names (mapcar (lambda (_) (varjo.utils:nth-or-self 0 _)) bindings))
         (dup-names (varjo.utils::find-duplicates binding-names)))
    (assert (not dup-names) () 'dup-names-in-let :names dup-names)
    (unless body
      (error 'body-block-empty :form-name 'let))
    (let ((updated-env (loop with updated-env = env
                             for (name glsl-name type &key read-only) in bindings
                             do (setf updated-env
                                      (varjo.internals:add-symbol-binding
                                       name
                                       (varjo.internals::v-make-value
                                        (varjo.internals::type-spec->type
                                         type
                                         (varjo.internals::%gl-flow-id!))
                                        env
                                        :glsl-name glsl-name
                                        :read-only read-only)
                                       updated-env))
                             finally (return updated-env))))
      (varjo.internals::merge-progn
       (varjo.internals::compile-progn body updated-env) env updated-env))))
