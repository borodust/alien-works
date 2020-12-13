(asdf:defsystem :alien-works
  :description "High-performance game foundation framework"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :pathname "src/"
  :depends-on (:alexandria :trivial-main-thread
               :cffi :cffi-c-ref :claw-utils :claw
               :static-vectors :uiop
               :claw-sdl :claw-glm :claw-physx :claw-filament
               :claw-assimp :claw-soil :defpackage-plus)
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
                             (:file "engine")))
               (:module "resources"
                :serial t
                :components ((:file "packages")
                             (:file "resources")
                             (:module "scene"
                              :serial t
                              :components ((:file "utils")
                                           (:file "mesh")
                                           (:file "scene")))))
               (:file "packages")))
