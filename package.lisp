;;;; package.lisp

(defpackage #:spot
  (:use #:cl)
  (:import-from :aviation-formulary
		:pp
		:point-serial-number
		:point-creation-time
		:point-creation-source
		:point-name
		:point-description
		:point-lat
		:point-lon
		:point-datum
		:point-alt
		:message-type
		:unix-time)
  (:import-from :geocode
		:lookup-location
		:extract-street-address-from-json
		:lat-lon-to-location
		:lat-lon-to-street-address)
  (:export :pp
	   :point-serial-number
	   :point-creation-time
	   :point-creation-source
	   :point-name
	   :point-description
	   :point-lat
	   :point-lon
	   :point-datum
	   :point-alt
	   :point-id
	   :point-messenger-id
	   :point-unix-time
	   :point-message-type
	   :point-model-id
	   :point-show-custom-msg
	   :point-date-time
	   :point-battery-state
	   :point-hidden
	   :spot-point
	   :lat-lon-to-street-address
	   :spot-street-address
	   :spot-short-print
	   :get-all-spots-api
	   :get-all-spots-local
	   :get-newest-spot-api
	   :get-newest-spot-local
	   :start-spot
	   :id
	   ))

