;;;; package.lisp

(defpackage #:spot
  (:use #:cl)
  (:export :pp
	   :lat-lon-to-street-address
	   :spot-street-address
	   :get-all-spots-api
	   :get-all-spots-local
	   :get-newest-spot-api
	   :get-newest-spot-local
	   :start-spot
	   :id
	   :messenger-id
	   :unix-time
	   :message-type
	   :latitude
	   :longtude
	   :model-id
	   :show-custom-msg
	   :date-time
	   :battery-state
	   :hidden
	   ))

