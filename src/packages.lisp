(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-umbrella-package (name &rest packages)
    (let ((existing-package (find-package name)))
      (when existing-package
        (do-symbols (sym existing-package)
          (unintern sym existing-package))))
    (let (import-from)
      (loop for name in packages
            for imported-package = (find-package name)
            do (let (package-symbols)
                 (do-external-symbols (sym imported-package)
                   (push (make-symbol (string sym)) package-symbols))
                 (push (cons name package-symbols) import-from)))
      `(defpackage ,name
         (:use)
         ,@(loop for (name . symbols) in import-from
                 collect `(:import-from ,name ,@symbols))
         (:export ,@(reduce #'union (mapcar #'cdr import-from)))))))


(define-umbrella-package :alien-works
  #:alien-works.math
  #:alien-works.host
  #:alien-works.audio
  #:alien-works.graphics
  #:alien-works.framework)
