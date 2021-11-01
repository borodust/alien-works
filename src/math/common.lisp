(cl:in-package :alien-works.math)


(define-symbol-macro +epsilon+
    (handler-case
        (%glm:glm+epsilon)
      (serious-condition ()
        `(%glm:glm+epsilon))))
