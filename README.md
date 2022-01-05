# alien-works

High-performance cross-platform game foundation framework. This isn't intended
as a full-featured game engine, but rather a stylobate for one.

### Principles

* Performance first
* Non-consing in tight loops
* Non-modular highly-coupled subsystems
* Multiplatform: Windows, MacOS, Linux, Android, iOS
* SBCL, CCL, LispWorks and ECL compatible
* Implementation purity last
  * Pure Lisp vs foreign solutions is the least concern
  * If any pure CL alternative exists with the same or better performance and
    similar feature set, it must replace foreign library
  * If foreign alternative exists with better performance and similar feature
    set, it must replace pure CL variant
* Explicit runtime and tooling systems
  * Ship only things required
* Package-level versioning
  * Stable interface is guaranteed only in versioned packages


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
* [SDL2](https://libsdl.org/)
* [Filament](https://github.com/google/filament)
* [Skia](https://skia.org/)
* [PhysX](https://github.com/NVIDIAGameWorks/PhysX)
* [Chipmunk](https://github.com/slembcke/Chipmunk2D)
* [OpenAL](https://github.com/kcat/openal-soft)
* [Opus](https://github.com/xiph/opus)
* [Assimp](https://github.com/assimp/assimp)
* [sndfile](https://github.com/libsndfile/libsndfile)
* [stb_image](https://github.com/nothings/stb/blob/master/stb_image.h)
* [ImGui](https://github.com/ocornut/imgui)
