(ql:quickload :spot)
(ql:quickload :creds)

(creds:load-creds)

(defun where-was-i-last-seen? ()
  (let ((loc (spot:get-newest-spot-api (creds:get-cred "spotglld") (creds:get-cred "spotpass"))))
    (concatenate 'string "address:" (spot:spot-street-address loc (creds:get-cred "googlegeoapi")) ", " (spot:pp loc))))
