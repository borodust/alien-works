(defpackage+-1:defpackage+ :alien-works
  (:inherit-from :alien-works.utils
                 #:memcpy)
  (:inherit
   :alien-works.math
   :alien-works.host
   :alien-works.graphics
   :alien-works.resources))
