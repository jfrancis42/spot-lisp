;;;; spot.lisp

(in-package #:spot)

(defparameter *spots* nil)
(defparameter *spots-lock* (bt:make-lock))
(defparameter *uptodate-thread* nil)

(defun bytes-to-ascii (bytelist)
  "Turn a list of bytes into string."
  (map 'string #'code-char bytelist))

(defun get-spot-locations (feed-glld &optional (passwd nil))
  "Get the last fifty entries as JSON data from the Spot server.
Accepts an optional password for password-protected location feeds."
  (let ((feed (concatenate 'string "https://api.findmespot.com/spot-main-web/consumer/rest-api/2.0/public/feed/"
			   feed-glld "/message.json")))
    (if passwd (setf feed (concatenate 'string feed "?feedPassword=" passwd)))
    (let ((result (drakma:http-request feed
                                       :method :get
                                       :accept "application/json"
                                       :content-type "application/json")))
      (if (> (length result) 0)
          (json:decode-json-from-string (bytes-to-ascii 
                                         (nth-value 0 result)))
          nil))))

(defun extract-spot-locations (spot-json)
  "Extract just the location data from the parsed JSON object returned
from the Spot API."
  (cdr (car (cdr (assoc :messages (cdr (car (cdr (car spot-json)))))))))

(defmethod make-location (l)
  "Turn a JSON location into a location object."
  (make-instance 'spot-point
                 :id (cdr (assoc :id l))
                 :messenger-id (cdr (assoc :messenger-id l))
                 :unix-time (cdr (assoc :unix-time l))
                 :message-type (cdr (assoc :message-type l))
                 :lat (cdr (assoc :latitude l))
                 :lon (cdr (assoc :longitude l))
                 :model-id (cdr (assoc :model-id l))
                 :show-custom-msg (cdr (assoc :show-custom-msg l))
                 :date-time (cdr (assoc :date-time l))
                 :battery-state (cdr (assoc :battery-state l))
                 :hidden (cdr (assoc :hidden l))
		 :creation-source point-spot
                 ))

(defmethod short-print ((n spot-point))
  "Print a short bit of location info."
  (format nil "lat:~A lon:~A type:~A batt:~A time:~A"
	  (point-lat n)
	  (point-lon n)
	  (message-type n)
	  (battery-state n)
	  (local-time:unix-to-timestamp (unix-time n))))

(defun sort-spots (location-list)
  "Sort a list of location objects."
  (let ((location-list-copy (copy-list location-list)))
    (sort location-list-copy #'<
          :key #'(lambda (n) (unix-time n)))))

(defun spot-street-address (spot google-api-key)
  "Convert a spot location into a street address."
  (geocode:extract-street-address-from-json (lookup-location spot google-api-key)))

(defun create-location-objects-from-list (location-list)
  "Takes a list of JSON locations (usually
from (extract-spot-locations)) and returns a list of location
objects."
  (map 'list #'(lambda (n) (make-location n)) location-list))

(defun get-all-spots-api (feed-glld feed-passwd)
  "Fetch the latest batch of spots from the Spot API."
  (sort-spots
   (create-location-objects-from-list
    (extract-spot-locations
     (get-spot-locations feed-glld feed-passwd)))))

(defun get-newest-spot-api (feed-glld feed-passwd)
  "Fetch the latest spot from the Spot API."
  (first
   (last
    (sort-spots
     (create-location-objects-from-list
      (extract-spot-locations
       (get-spot-locations feed-glld feed-passwd)))))))

(defun up-to-dater (feed-glld feed-passwd)
  "Keep the data structure up to date."
  (loop
     (bt:with-lock-held (*spots-lock*)
       (setf *spots* (get-all-spots-api feed-glld feed-passwd)))
     (sleep (* 60 2.5))))

(defun start-spot (feed-glld feed-passwd)
  "Start the thread that reads the latest API data from spot.com every
2.5 minutes. This keeps the *spots* list up-to-date."
  (setf *uptodate-thread* (bt:make-thread (lambda () (up-to-dater feed-glld feed-passwd)) :name "up-to-dater"))
  (format t "spot update thread is running...~%"))

(defun get-all-spots-local ()
  "Returns a list of all spots (assuming the thread is running)."
  *spots*)

(defun get-newest-spot-local ()
  "Returns the newest spot (assuming the thread is running)."
  (first (last *spots*)))
