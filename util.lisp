;;; UTILITIES

(in-package #:compost)

(defun pw-digest (str)
  (flexi-streams:octets-to-string 
   (ironclad:digest-sequence
    :sha256
    (flexi-streams:string-to-octets str :external-format :utf-8))
   :external-format :latin1))

(defun make-uid (&optional (string ""))
  (remove #\=  (base64:string-to-base64-string 
                (format nil "~a~a"
                        (gensym string)
                        (get-universal-time)))))



