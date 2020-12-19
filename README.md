# alien-works

WIP high-performance mobile-capable game foundation framework. This is not a
game engine, but rather a stereobate for one.

### Principles

* Performance first
* Non-consing
* Non-modular/tightly-coupled
* Multiplatform: Windows, MacOS, Linux, Android, iOS
* SBCL, CCL and ECL compatible
* Implementation purity last (pure Lisp vs foreign solutions is the least concern)
  * If any pure CL alternative exists with the same or better performance and
similar feature set, it must replace foreign library
  * If foreign alternative exists with better performance and similar feature set,
it must replace pure CL variant


### Capabilities

* SIMD-optimized math
* 3D and 2D graphics
* 3D and 2D physics
* Spatial audio
* Keyboard/Mouse, Controller/Joystick
* Resource handling
* Editor/Debug UI


### Foundation

* [GLM](https://github.com/g-truc/glm)
* [SDL](https://libsdl.org/)
* [Filament](https://github.com/google/filament)
* [Skia](https://skia.org/)
* [PhysX](https://github.com/NVIDIAGameWorks/PhysX)
* [Chipmunk](https://github.com/slembcke/Chipmunk2D)
* [OpenAL](https://github.com/kcat/openal-soft)
* [Assimp](https://github.com/assimp/assimp)
* [sndfile](https://github.com/libsndfile/libsndfile)
* [stb_image](https://github.com/nothings/stb/blob/master/stb_image.h)
* [Nuklear](https://github.com/Immediate-Mode-UI/Nuklear)
