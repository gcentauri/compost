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

(defun join-strings (sep strings)
  (with-output-to-string (out)
    (loop :for (str . more) :on strings
          :do (princ str out)
          :when more :do (princ sep out))))

(let ((timezone-names nil))
  (defun timezone-names ()
    (if timezone-names timezone-names
        (setf timezone-names
              (sort  (loop :for k :being :the :hash-keys :of local-time::*location-name->timezone* 
                           :collect k)
                     #'string<)))))

(defun timestring (universal-time zone)
  "Given a UNIVERSAL-TIME and a string naming a timezone, return a
  nice looking string  representing the time."
  (let ((format-list
          (if (< 10 (nth-value 1 (decode-universal-time universal-time)))
              '(:long-weekday ", " :long-month " " :day " at " :hour12 ":" :min :ampm)
              '(:long-weekday ", " :long-month " " :day " at " :hour12 ":0" :min :ampm))))
    (local-time:format-timestring
     nil
     (local-time:universal-to-timestamp universal-time)
     :format format-list
     :timezone (local-time:find-timezone-by-location-name zone))))

(defun short-timestring (universal-time zone)  
  (let ((format-list
          (if (< 10 (nth-value 1 (decode-universal-time universal-time)))
              '( :year "-" :month "-" :day " " :hour12 ":" :min :ampm)
              '( :year "-" :month "-" :day " " :hour12 ":0" :min :ampm))))
    (local-time:format-timestring
     nil
     (local-time:universal-to-timestamp universal-time)
     :format format-list
     :timezone (local-time:find-timezone-by-location-name zone))))


(defun remove-carriage-return (string)
  "removes #\Return from strings"
  (remove #\return string< ))
