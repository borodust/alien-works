(cl:defpackage :%alien-works.math
  (:use)
  (:export #:vec2-element-ptr
           #:vec2-ptr

           #:vec3-element-ptr
           #:vec3-ptr))


(cl:defpackage :alien-works.math
  (:local-nicknames (:a :alexandria)
                    (:! :alien-works.utils.empty)
                    (:u :alien-works.utils)
                    (:%math :%alien-works.math))
  (:use :cl)
  (:export #:vec2
           #:make-vec2
           #:destroy-vec2
           #:with-vec2

           #:vec3
           #:make-vec3
           #:destroy-vec3
           #:with-vec3
           #:with-vec3*
           #:vec3-add
           #:vec3-mult
           #:vec3-dot
           #:vec3-cross

           #:vec4
           #:make-vec4
           #:destroy-vec4
           #:with-vec4

           #:mat3
           #:make-mat3
           #:make-mat3-from-basis
           #:destroy-mat3
           #:with-mat3
           #:with-mat3-from-basis

           #:mat4
           #:make-mat4
           #:destroy-mat4
           #:with-mat4
           #:with-mat4*
           #:rotate-mat4
           #:translate-mat4
           #:scale-mat4
           #:mat4-mult

           #:quat
           #:make-quat
           #:destroy-quat
           #:with-quat
           #:mat3->quat
           #:normalize-quat
           #:positivize-quat
           #:negate-quat))
