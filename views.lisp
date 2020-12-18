(in-package #:compost)

(defmacro defpage  (name lambda-list (&key stylesheets scripts (title "")) &body body)
  (let ((page-name (intern (format nil "PAGE/~a" name))))
    `(defun ,page-name ,lambda-list
       (with-html-string
         (:doctype)
         (:html
          (:head
           (:title ,title)
           (dolist (css (list ,@stylesheets)) 
             (:link :rel "stylesheet" :href css)))
          (:body
           ,@body
           (dolist (js (list ,@scripts))
             (:script :src js))))))))

(defmacro defview (name lambda-list &body body)
  (let ((view-name (intern (format nil "VIEW/~a" name))))
    `(defun ,view-name ,lambda-list
       (with-html
         ,@body))))


(defpage login () (:title "Compost - Login")
  (:form :method "POST" :action "/login"
              (:input :placeholder "Username" :type "text" :name "username")
              (:input :placeholder "Password" :type "password" :name "password")
              (:button :type "submit" :class "button" "Login")))


(defpage frontpage (user) (:title "Compost")
  (:h1 "hey" (user-name user)))
