(ql:quickload :spot)
(ql:quickload :creds)

(creds:load-creds)

(defun where-was-i-last-seen? ()
  (let ((loc (spot:get-newest-spot-api (creds:get-cred "spotglld") (creds:get-cred "spotpass"))))
    (concatenate 'string "address:" (spot:spot-street-address loc (creds:get-cred "googlegeoapi")) ", " (spot:pp loc))))

(defun watch-spots ()
  "Loop forever and print each new Spot location as it rolls in (must have already started (start-spot)."
  (let ((old-loc nil) (new-loc t))
    (loop
       (setf new-loc (spot:get-newest-spot-local))
       (when (null old-loc)
	 (setf old-loc new-loc)
	 (setf (spot:point-id old-loc) 0))
       (when (not (= (spot:point-id old-loc) (spot:point-id new-loc)))
	 (setf old-loc new-loc)
	 (spot:pp new-loc)
	 (format t "~A~%" (spot:spot-street-address new-loc (creds:get-cred "googlegeoapi")))
	 (sleep 1)))))
