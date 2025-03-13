;;;; spot.asd

(asdf:defsystem #:spot
  :description "A library for extracting SPOT data from the findmespot.com API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
	       #:babel
	       #:jeffutils
               #:cl-json
	       #:geocode
               #:local-time
               #:bordeaux-threads
	       #:aviation-formulary)
  :serial t
  :components ((:file "package")
               (:file "spot")))
