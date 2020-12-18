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

(defun split-string (delimiter string)
  (labels ((rec (acc start)
             (if-let (found (search delimiter string :start2 start))
               (rec (cons (subseq string start found) acc)
                    (+ found (length delimiter)))
               (reverse (cons (subseq string start) acc)))))
    (rec nil 0)))



