(asdf:defsystem :alien-works
  :description "High-performance game foundation framework"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :pathname "src/"
  :depends-on (:alexandria :trivial-main-thread
               :cffi :cffi-c-ref :claw-utils :claw
               :claw-sdl :claw-mathfu :claw-physx :claw-filament)
  :serial t
  :components ((:file "packages")
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
                              :components ((:file "math")
                                           (:file "engine")
                                           (:file "entity")
                                           (:file "renderer")
                                           (:file "swap-chain")
                                           (:file "view")
                                           (:file "scene")
                                           (:file "camera")
                                           (:file "skybox")))
                             (:file "engine")))
               (:file "proof-of-concept")))
