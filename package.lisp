;;;; package.lisp

(defpackage #:spot
  (:use #:cl)
  (:import-from :aviation-formulary
		:pp
		:point-name
		:point-description
		:point-datum
		:spot-point
		:point-lat
		:point-lon
		:message-type
		:battery-state
		:unix-time
		:creation-source
		:point-spot)
  (:import-from :geocode
		:lookup-location
		:extract-street-address-from-json
		:lat-lon-to-location
		:lat-lon-to-street-address)
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

