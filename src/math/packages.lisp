(cl:defpackage :alien-works.math
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty)
                    (:u :alien-works.utils))
  (:use :cl)
  (:export #:vec2
           #:make-vec2
           #:destroy-vec2
           #:with-vec2

           #:vec3
           #:make-vec3
           #:destroy-vec3
           #:with-vec3

           #:vec4
           #:make-vec4
           #:destroy-vec4
           #:with-vec4

           #:mat4
           #:create-mat4
           #:destroy-mat4
           #:with-mat4
           #:rotate-mat4))
