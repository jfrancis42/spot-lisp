(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :local-time)
(ql:quickload :bordeaux-threads)
(ql:quickload :marshal)
(ql:quickload :quicklisp-slime-helper)

(defparameter *glld* "PlaceYourGLLDThatSpotGivesYouHere")
(defparameter *google-api-key* "PlaceYourGoogleGeocoderAPIKeyHere")
(defparameter *feedpassword* "PlaceYourSpotSharedSitePasswordHere(orNilIfNoPassword)")
(defparameter *spotdb* "~/spot.db")
(defparameter *sleeptime* 30)
(defparameter *spots* nil)
(defparameter *spots-lock* (bt:make-lock))
(defparameter *uptodate-thread* nil)

(defun bytes-to-ascii (bytelist)
  "Turn a list of bytes into string (for some very annoying reason,
drakma:http-request returns the body as a list of bytes instead of a
string)."
  (map 'string #'code-char bytelist))

(defun get-spot-locations (glld &optional (passwd nil))
  "Get the last fifty entries as JSON data from the Spot server.
Accepts an optional password for password-protected location feeds."
  (let ((feed (concatenate 'string "https://api.findmespot.com/spot-main-web/consumer/rest-api/2.0/public/feed/"
			   glld "/message.json")))
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

(defclass location ()
  ((id :accessor id
         :initarg :id
         :initform nil)
   (messenger-id :accessor messenger-id
                 :initarg :messenger-id
                 :initform nil)
   (unix-time :accessor unix-time
        :initarg :unix-time
        :initform nil)
   (message-type :accessor message-type
        :initarg :message-type
        :initform nil)
   (latitude :accessor latitude
        :initarg :latitude
        :initform nil)
   (longitude :accessor longitude
        :initarg :longitude
        :initform nil)
   (model-id :accessor model-id
        :initarg :model-id
        :initform nil)
   (show-custom-msg :accessor show-custom-msg
        :initarg :show-custom-msg
        :initform nil)
   (date-time :accessor date-time
        :initarg :date-time
        :initform nil)
   (battery-state :accessor battery-state
        :initarg :battery-state
        :initform nil)
   (hidden :accessor hidden
        :initarg :hidden
        :initform nil)
   ))

(defmethod ms:class-persistant-slots ((self location))
  "This allows for object marshalling."
  '(id messenger-id unix-time message-type latitude longitude model-id show-custom-msg battery-state hidden))

(defmethod make-location (l)
  "Turn a JSON location into a location object."
  (make-instance 'location
                 :id (cdr (assoc :id l))
                 :messenger-id (cdr (assoc :messenger-id l))
                 :unix-time (cdr (assoc :unix-time l))
                 :message-type (cdr (assoc :message-type l))
                 :latitude (cdr (assoc :latitude l))
                 :longitude (cdr (assoc :longitude l))
                 :model-id (cdr (assoc :model-id l))
                 :show-custom-msg (cdr (assoc :show-custom-msg l))
                 :date-time (cdr (assoc :date-time l))
                 :battery-state (cdr (assoc :battery-state l))
                 :hidden (cdr (assoc :hidden l))
                 ))

(defmethod pp ((n location))
  "Pretty print a location."
  (format nil "lat:~A lon:~A type:~A batt:~A time:~A"
	  (latitude n)
	  (longitude n)
	  (message-type n)
	  (battery-state n)
	  (local-time:unix-to-timestamp (unix-time n))))

(defun sort-spots (location-list)
  "Sort a list of location objects."
  (let ((location-list-copy (copy-list location-list)))
    (sort location-list-copy #'<
          :key #'(lambda (n) (unix-time n)))))

(defun write-spots-to-file (f spots)
  "Write the list of spot location objects to a file."
  (with-open-file (stream f
			  :direction :output
			  :if-exists :supersede)
    (print (ms:marshal spots) stream)))

(defun read-spots-from-file (f)
  "Read in a previously save list of spot location objects."
  (let ((spt nil))
    (if (probe-file f)
	(with-open-file (stream f :direction :input)
	  (setf spt (ms:unmarshal (read stream))))
	nil)))

(defun lookup-location (loc)
  "Use the Google Geocoding API to do a reverse geocode lookup (ie,
convert lat lon to address)."
  (let* ((url (concatenate 'string "https://maps.googleapis.com/maps/api/geocode/json?latlng="
			   (format nil "~A" (latitude loc)) ","
			   (format nil "~A" (longitude loc))
			   "&key=" *google-api-key*))
	 (result (drakma:http-request url
				      :method :get
				      :accept "application/json"
				      :content-type "application/json")))
    (if (> (length result) 0)
	(json:decode-json-from-string (bytes-to-ascii 
				       (nth-value 0 result)))
	nil)))

(defun street-address (geocoded-result)
  "Extract the formatted street address from (lookup-location)
result."
  (cdr (assoc :formatted--address (cadar geocoded-result))))

(defun lat-lon-to-street-address (lat lon)
  "Convert an arbitrary lat/lon into a street address."
  (street-address (lookup-location (make-instance 'location :latitude lat :longitude lon))))

(defun create-location-objects-from-list (location-list)
  "Takes a list of JSON locations (usually
from (extract-spot-locations)) and returns a list of location
objects."
  (map 'list #'(lambda (n) (make-location n)) location-list))

(defun up-to-dater ()
  "Keep the data structure up to date."
  (loop
     (bt:with-lock-held (*spots-lock*)
       (setf *spots*
             (sort-spots
              (create-location-objects-from-list
               (extract-spot-locations
                (get-spot-locations *glld* *feedpassword*))))))
     (sleep (* 60 2.5))))

(defun start-spot ()
  "Start the thread that reads the latest API data from spot.com every
2.5 minutes. This keeps the *spots* list up-to-date."
  (setf *uptodate-thread* (bt:make-thread (lambda () (up-to-dater)) :name "up-to-dater"))
  (format t "spot update thread is running...~%"))

(defun watch-spots ()
  "Loop forever and print each new Spot location as it rolls in."
  (let ((old-loc nil) (new-loc t))
    (loop
       (bt:with-lock-held (*spots-lock*) (setf new-loc (pp (first (last *spots*))))
			  (if (not (equal old-loc new-loc))
			      (progn
				(setf old-loc new-loc)
				(write-spots-to-file (format nil "~A.log" (local-time:unix-to-timestamp (unix-time (first (last *spots*))))) *spots*)
				(format t "~A ~A~%" new-loc (street-address (lookup-location (first (last *spots*)))) ))))
       (sleep *sleeptime*))))

(defun one-time ()
  "Load the largest chunk of spots allowed and write them to a
file. Designed to be run from something like cron after doing a
save-lisp-and-die."
  (setf *spots*
	(sort-spots
	 (create-location-objects-from-list
	  (extract-spot-locations
	   (get-spot-locations *glld* *feedpassword*)))))
  (write-spots-to-file (format nil "~A.log" (local-time:unix-to-timestamp (unix-time (first (last *spots*))))) *spots*))

(defun make-executable ()
  "Write an executable to disk as 'spots' with the current logins,
passwords, etc."
  (sb-ext:save-lisp-and-die "spots" :toplevel #'one-time :executable t :purify t :compression 9))
