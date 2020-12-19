(in-package #:compost)

(defparameter +session-cookie-key+
  "session")

(defvar *user* nil
  "The authenticated user for this request.")

;;; HTTP Utilities 

(defun get-header (header)
  (gethash header (getf *req* :headers)))

(defun parse-cookie (cookie-string)
  "transforms a cookie string into an alist"
  (loop :for pair :in  (split-string "; " cookie-string)
        :collect (split-string "=" pair)))

(defun get-cookie (cookie-key)
  (when-let ((cookie (get-header "cookie")))
    (second (assoc cookie-key (parse-cookie cookie) :test #'equal))))

(defun find-user-session ()
  (when-let (session-cookie (get-cookie +session-cookie-key+))
    (when-let (session (session-by-cookie session-cookie))
      (session-user session))))


;;; Routes

(defmacro defroute-auth (method path &body body)
  `(defroute ,method ,path
     (if-let (*user* (find-user-session))
       (progn ,@body)
       (http-redirect "/login"))))

(defroute :get "/css/main.css"
  (http-ok "text/css" (main-css)))

(defroute :get "/login"
  (http-ok "text/html" (page/login)))

(defroute :post "/login"
  (if-let (user (login-user (getf *body* :username)
                            (getf *body* :password)))
    (let ((session (db:with-transaction () (make-session user))))
      (lzb:add-header :set-cookie (cookie-header-value session))
      (lzb:http-redirect "/"))
    (http-redirect "/login")))

(defroute-auth :get "/"
  (http-ok "text/html" (page/frontpage)))

(defroute-auth :get "/topic/view/:name"
  (if-let (topic (topic-by-name name))
    (http-ok "text/html" (page/topic topic))
    (http-err 404 "Not Found")))

(defroute-auth :post "/topic/add"
  (when-let (ok (not (topic-by-name (getf *body* :name))))
    (db:with-transaction ()
      (make-instance 'topic :name (getf *body* :name))))
  (http-redirect "/"))

(defroute-auth :get "/topic/new-post/:topic-id"
  (if-let (topic (db:store-object-with-id (parse-integer topic-id)))
    (http-ok "text/html" (page/new-post topic))
    (http-err 404 "Not Found")))

(defroute-auth :post "/topic/new-post/:topic-id"
  (if-let (topic (db:store-object-with-id (parse-integer topic-id)))
    (let ((post 
            (db:with-transaction ()
              (make-instance 'title-post
                             :title (getf *body* :title)
                             :user *user*
                             :render-style :markdown
                             :topic topic
                             :text (clean-crlf  (getf *body* :text ))))))
      (http-redirect (path-to post)))
    (http-err 404 "Topic not found")))

(defroute-auth :get "/post/view/:postid"
  (if-let (post (db:store-object-with-id (parse-integer postid)))
    (http-ok "text/html" (page/post post))
    (http-err 404 "Not Found")))
