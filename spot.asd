;;;; spot.asd

(asdf:defsystem #:spot
  :description "A library for extracting SPOT data from the findmespot.com API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
               #:cl-json
               #:local-time
               #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "spot")))
