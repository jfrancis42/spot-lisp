;;;; package.lisp

(defpackage #:spot
  (:use #:cl)
  (:import-from :aviation-formulary
		:pp
		:spot-point
		:point-lat
		:point-lon
		:message-type
		:battery-state
		:unix-time
		:creation-source
		:point-spot)
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

