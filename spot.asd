;;;; spot.asd

(asdf:defsystem #:spot
  :description "Describe spot here"
  :author "Your Name <your.name@example.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
               #:cl-json
               #:local-time
               #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "spot")))

