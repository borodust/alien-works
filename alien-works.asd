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
  :components ((:file "proof-of-concept")))
