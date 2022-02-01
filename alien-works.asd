(pushnew :cl-opengl-no-preload *features*)

(asdf:defsystem :alien-works
  :description "High-performance game foundation framework"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :pathname "src/"
  :depends-on (;; foreign api
               #:alien-works-foundation
               #:aw-sdl
               #:aw-glm
               #:aw-filament/runtime
               #:aw-stb/image
               #:aw-physx
               #:aw-chipmunk
               #:aw-skia
               #:aw-openal
               #:aw-opus
               (:feature :android #:cl-opengl/es2)
               (:feature (:not :android) #:cl-opengl)
               ;; ffi
               #:static-vectors
               #:claw-utils
               #:cffi
               #:cffi-c-ref
               ;; generic utility
               #:uiop
               #:alexandria
               #:trivial-main-thread
               #:defpackage-plus
               #:trivial-gray-streams
               #:flexi-streams
               #:bordeaux-threads
               #:atomics
               #:cl-muth)
  :serial t
  :components ((:module "utils"
                :serial t
                :components ((:file "utils")))
               (:module "math"
                :serial t
                :components ((:file "packages")
                             (:file "common")
                             (:file "vec2")
                             (:file "vec3")
                             (:file "vec4")
                             (:file "quat")
                             (:file "mat3")
                             (:file "mat4")
                             (:file "math")))
               (:module "host"
                :serial t
                :components ((:file "packages")
                             (:file "system/linux" :if-feature (:and :linux (:not :android)))
                             (:file "system/android" :if-feature :android)
                             (:file "system/windows" :if-feature :windows)
                             (:file "host")))
               (:module "audio"
                :serial t
                :components ((:file "packages")
                             (:module "openal"
                              :components ((:file "openal")))
                             (:module "opus"
                              :components ((:file "opus")))
                             (:file "audio")))
               (:module "graphics"
                :serial t
                :components ((:file "packages")
                             (:file "surface")
                             (:module "filament"
                              :serial t
                              :components ((:file "utils")
                                           (:file "math")
                                           (:file "box")
                                           (:file "engine")
                                           (:file "material")
                                           (:file "transform")
                                           (:file "buffer")
                                           (:file "entity")
                                           (:file "renderable")
                                           (:file "renderer")
                                           (:file "swap-chain")
                                           (:file "view")
                                           (:file "scene")
                                           (:file "camera")
                                           (:file "skybox")
                                           (:file "texture")
                                           (:file "light")))
                             (:module "skia"
                              :serial t
                              :components ((:file "skia")))
                             (:file "canvas")
                             (:file "engine")))
               (:module "physics"
                :serial t
                :components ((:file "packages")
                             (:module "physx"
                              :serial t
                              :components ((:file "math")
                                           (:file "foundation")
                                           (:file "vdb")
                                           (:file "physics")
                                           (:file "dispatcher")
                                           (:file "material")
                                           (:file "scene")
                                           (:file "actor")))
                             (:file "physics")))
               (:module "framework"
                :serial t
                :components ((:file "packages")
                             (:file "framework")))
               (:file "packages")))


(asdf:defsystem :alien-works/tools
  :description "High-performance game foundation framework"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:alien-works
               #:alien-works-foundation/tools
               #:aw-filament/tools
               #:aw-assimp
               #:aw-stb/image-write
               #:defpackage-plus)
  :serial t
  :pathname "tools/"
  :components ((:module "graphics"
                :serial t
                :components ((:file "packages")
                             (:module "filament"
                              :components ((:file "material")))
                             (:file "graphics")))
               (:module "resources"
                :serial t
                :components ((:file "packages")
                             (:file "image")
                             (:file "resources")
                             (:module "scene"
                              :serial t
                              :components ((:file "utils")
                                           (:file "material")
                                           (:file "mesh")
                                           (:file "scene")))
                             (:file "gltf")))
               (:module "ui"
                :serial t
                :components ((:file "packages")
                             (:module "imgui"
                              :serial t
                              :components ((:file "imgui")))
                             (:file "ui")))
               (:file "packages")))
