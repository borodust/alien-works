(asdf:defsystem :alien-works
  :description "High-performance game foundation framework"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :pathname "src/"
  :depends-on (:alexandria :uiop
               :trivial-main-thread :static-vectors
               :cffi :cffi-c-ref :claw-utils :defpackage-plus
               :aw-sdl :aw-glm :aw-filament/runtime :aw-stb/image
               :aw-physx :aw-imgui :aw-skia :trivial-gray-streams
               :cl-opengl :bordeaux-threads :atomics)
  :serial t
  :components ((:module "utils"
                :serial t
                :components ((:file "utils")))
               (:module "math"
                :serial t
                :components ((:file "packages")
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
                             (:file "system/linux" :if-feature :linux)
                             (:file "system/android" :if-feature :android)
                             (:file "host")))
               (:module "physics"
                :serial t
                :components ((:file "packages")
                             (:module "physx"
                              :components ((:file "math")
                                           (:file "foundation")
                                           (:file "vdb")
                                           (:file "physics")
                                           (:file "dispatcher")
                                           (:file "material")
                                           (:file "scene")
                                           (:file "actor")))
                             (:file "physics")))
               (:module "graphics"
                :serial t
                :components ((:file "packages")
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
                             (:file "surface")
                             (:file "engine")))
               (:file "packages")))


(asdf:defsystem :alien-works/tools
  :description "High-performance game foundation framework"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:alien-works
               :aw-filament/tools :aw-assimp :aw-stb/image-write
               :defpackage-plus)
  :serial t
  :pathname "src/support/"
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
                                           (:file "scene")))))
               (:file "packages")))
