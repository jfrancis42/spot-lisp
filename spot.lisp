;;;; spot.lisp

(in-package #:spot)

(defparameter *spots* nil)
(defparameter *spots-lock* (bt:make-lock))
(defparameter *uptodate-thread* nil)
(defconstant point-spot 3)

(defmacro cdr-assoc (name alist)
  "Replaces '(cdr (assoc name alist))' because it's used a bajillion
times when doing API stuff."
  `(cdr (assoc ,name ,alist :test #'equal)))

;;  Descendant of aviation-formulary's 2d-point.  Adds fields for SPOT
;;  service.
(defclass spot-point (af:2d-point)
  ((id :accessor point-id
       :initarg :id
       :initform nil)
   (messenger-id :accessor point-messenger-id
                 :initarg :messenger-id
                 :initform nil)
   (unix-time :accessor point-unix-time
              :initarg :unix-time
              :initform nil)
   (message-type :accessor point-message-type
		 :initarg :message-type
		 :initform nil)
   (model-id :accessor point-model-id
             :initarg :model-id
             :initform nil)
   (show-custom-msg :accessor point-show-custom-msg
		    :initarg :show-custom-msg
		    :initform nil)
   (date-time :accessor point-date-time
              :initarg :date-time
              :initform nil)
   (battery-state :accessor point-battery-state
		  :initarg :battery-state
		  :initform nil)
   (hidden :accessor point-hidden
           :initarg :hidden
           :initform nil)
   ))

(defmethod point-serialize ((p spot-point))
  "Serialize a SPOT point."
  (append
   (list
    '(type spot-point)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'datum (point-datum p))
    (list 'id (point-id p))
    (list 'unix-time (unix-time p))
    (list 'message-type (point-message-type p))
    (list 'model-id (point-model-id p))
    (list 'show-custom-msg (point-show-custom-msg p))
    (list 'date-time (point-date-time p))
    (list 'battery-state (point-battery-state p))
    (list 'hidden (point-hidden p))
    )
   (af:point-metadata-serialize p)))

(defmethod pp ((p spot-point))
  "Pretty print a spot point."
  (format t "Name:  ~A~%" (point-name p))
  (format t "Descr:  ~A~%" (point-description p))
  (format t "Lat:  ~F~%" (point-lat p))
  (format t "Lon:  ~F~%" (point-lon p))
  (format t "Id:  ~F~%" (point-id p))
  (format t "Unix-Time:  ~F~%" (point-unix-time p))
  (format t "Message Type:  ~F~%" (point-message-type p))
  (format t "Model Id:  ~F~%" (point-model-id p))
  (format t "Show Custom Msg:  ~F~%" (point-show-custom-msg p))
  (format t "Date-Time:  ~F~%" (point-date-time p))
  (format t "Battery State:  ~F~%" (point-battery-state p))
  (format t "Hidden:  ~F~%" (point-hidden p))
  (format t "Datum:  ~A~%" (point-datum p)))

(defmethod point-deserialize-method ((p spot-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (point-metadata-deserialize-method p point-data)
  (mapcar #'(lambda (n)
	      (cond
		((equal (first n) 'lat)
		 (setf (point-lat p) (second n)))
		((equal (first n) 'lon)
		 (setf (point-lon p) (second n)))
		((equal (first n) 'id)
		 (setf (point-id p) (second n)))
		((equal (first n) 'unix-time)
		 (setf (point-unix-time p) (second n)))
		((equal (first n) 'message-type)
		 (setf (point-message-type p) (second n)))
		((equal (first n) 'model-id)
		 (setf (point-model-id p) (second n)))
		((equal (first n) 'show-custom-msg)
		 (setf (point-show-custom-msg p) (second n)))
		((equal (first n) 'date-time)
		 (setf (point-date-time p) (second n)))
		((equal (first n) 'battery-state)
		 (setf (point-battery-state p) (second n)))
		((equal (first n) 'hidden)
		 (setf (point-hidden p) (second n)))
		((equal (first n) 'datum)
		 (setf (point-datum p) (second n)))
		))
	  point-data))

(defun get-spot-locations (feed-glld &optional (passwd nil))
  "Get the last fifty entries as JSON data from the Spot server.
Accepts an optional password for password-protected location feeds."
  (let
      ((feed
	(concatenate
	 'string
	 "https://api.findmespot.com/spot-main-web/consumer/rest-api/2.0/public/feed/"
	 feed-glld "/message.json")))
    (if passwd
	(setf feed (concatenate 'string feed "?feedPassword=" passwd)))
    (let ((result (drakma:http-request
		   feed
                   :method :get
                   :accept "application/json"
                   :content-type "application/json")))
      (if (> (length result) 0)
          (json:decode-json-from-string
	   (babel:octets-to-string 
            (nth-value 0 result)))
          nil))))

(defun extract-spot-locations (spot-json)
  "Extract just the location data from the parsed JSON object returned
from the Spot API."
  (let ((count (jeff:cdr-assoc :count
			       (jeff:cdr-assoc :feed-message-response
					       (jeff:cdr-assoc :response spot-json)))))
    (cond
      ((null count)
       nil)
      ((= 0 count)
       nil)
      ((= 1 count)
       (list
	(cdr-assoc :message
		   (cdr-assoc :messages
			      (cdr-assoc :feed-message-response
					 (cdr-assoc :response spot-json))))))
      (t
       (cdr-assoc :message
		  (cdr-assoc :messages
			     (cdr-assoc :feed-message-response
					(cdr-assoc :response spot-json))))))))
       

(defmethod make-location (l)
  "Turn a JSON location into a location object."
  (make-instance 'spot-point
                 :id (cdr-assoc :id l)
                 :messenger-id (cdr-assoc :messenger-id l)
                 :unix-time (cdr-assoc :unix-time l)
                 :message-type (cdr-assoc :message-type l)
                 :lat (cdr-assoc :latitude l)
                 :lon (cdr-assoc :longitude l)
                 :model-id (cdr-assoc :model-id l)
                 :show-custom-msg (cdr-assoc :show-custom-msg l)
                 :date-time (cdr-assoc :date-time l)
                 :battery-state (cdr-assoc :battery-state l)
                 :hidden (cdr-assoc :hidden l)
		 :creation-source point-spot
                 ))

(defmethod short-print ((n spot-point))
  "Print a short bit of location info."
  (format nil "lat:~A lon:~A type:~A batt:~A time:~A"
	  (point-lat n)
	  (point-lon n)
	  (point-message-type n)
	  (point-battery-state n)
	  (local-time:unix-to-timestamp (point-unix-time n))))

(defun sort-spots (location-list)
  "Sort a list of location objects."
  (let ((location-list-copy (copy-list location-list)))
    (sort location-list-copy #'<
          :key #'(lambda (n) (point-unix-time n)))))

(defun spot-street-address (spot google-api-key)
  "Convert a spot location into a street address."
  (geocode:extract-street-address-from-json
   (lookup-location spot google-api-key)))

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
  "Keep the data structure up to date. This fetches new data at the
maximum rate specified by Spot (every 2.5 minutes)."
  (loop
     (bt:with-lock-held (*spots-lock*)
       (setf *spots* (get-all-spots-api feed-glld feed-passwd)))
     (sleep (* 60 2.5))))

(defun start-spot (feed-glld feed-passwd)
  "Start the thread that reads the latest API data from spot.com every
2.5 minutes. This keeps the *spots* list up-to-date."
  (setf *uptodate-thread*
	(bt:make-thread
	 (lambda ()
	   (up-to-dater feed-glld feed-passwd)) :name "up-to-dater"))
  (format t "spot update thread is running...~%"))

(defun get-all-spots-local ()
  "Returns a list of all spots (assuming the thread is running)."
  *spots*)

(defun get-newest-spot-local ()
  "Returns the newest spot (assuming the thread is running)."
  (first (last *spots*)))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:

;; ?startDate=2012-07-03T00:00:00-0000&endDate=2012-08-02T00:00:00-0000
