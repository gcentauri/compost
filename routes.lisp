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

(defun multipart-field-values (&rest keys)
  (loop :for field :in *body*
        :when (member (getf field :name) keys :test #'string-equal)
          :collect (getf field :body)))

(defun multipart-files ()
  (remove-if-not (lambda (ls) (getf ls :filename)) *body*))

(defroute-auth :post "/topic/new-post/:topic-id"
  (if-let (topic (db:store-object-with-id (parse-integer topic-id)))
    (let ((post 
            (db:with-transaction ()
              (let ((post 
                      (destructuring-bind (title text) (multipart-field-values "title" "text")
                        (make-instance 'title-post
                                       :title title
                                       :user *user*
                                       :render-style :markdown
                                       :topic topic
                                       :text (remove-carriage-return text)))))
                (dolist (file (multipart-files))
                  (db:make-blob-from-file (getf file :body) 'attachment
                                          :filename (getf file :filename)
                                          :post post
                                          :type (getf file :content-type)))
                post))))
      (http-redirect (path-to post)))
    (http-err 404 "Topic not found")))

(defroute-auth :get "/post/view/:postid"
  (if-let (post (db:store-object-with-id (parse-integer postid)))
    (http-ok "text/html" (page/post post))
    (http-err 404 "Not Found")))

(defroute-auth :post "/post/reply/:postid"
  (if-let (post (db:store-object-with-id (parse-integer postid)))
    (let ((top-post-id (db:store-object-id (find-root-post post))))
      (let ((new-post
              (db:with-transaction ()
                (make-instance 'reply-post
                               :predecessor post
                               :user *user*
                               :render-style :markdown
                               :text (remove-carriage-return (getf *body* :text))))))
        (http-redirect (path-to new-post))))
    (http-err 404 "Not Found")))

(defroute-auth :get "/file/:blob-id/:filename"
  (if-let (blob (db:store-object-with-id (parse-integer blob-id)))
    (http-ok (db:blob-mime-type blob)
             (alexandria:read-file-into-byte-vector
              (db:blob-pathname blob)))
    (http-err 404 "Not Found")))

(defroute-auth :get "/user/profile"
  (http-ok "text/html" (page/profile)))

(defroute-auth :post "/user/name"
  (db:with-transaction ()
    (setf (user-name *user*)            ; uniqueness should be covered by db
          (getf *body* :name)))
  (http-redirect "/user/profile"))


(defroute-auth :post "/user/password"
  (if (and
       (equal (pw-hash *user*)
              (pw-digest (getf *body* :old-password)))
       (equal (getf *body* :new-password)
              (getf *body* :repeat-password)))
      (progn
        (db:with-transaction ()
          (setf (pw-hash *user*)
                (pw-digest (getf *body* :new-password))))
        (http-redirect "/user/profile"))))

(defroute-auth :get "/post/edit/:post-id"
  (if-let (post (db:store-object-with-id (parse-integer post-id)))
    (if (eql (post-user post) *user*)
        (http-ok "text/html" (page/edit-post post))
        (http-err 401 "Unauthorized"))
    (http-err 404 "Not Found")))

(defroute-auth :post "/post/edit/:post-id"
  (if-let (post (db:store-object-with-id (parse-integer post-id)))
    (if (eql (post-user post) *user*)
        (progn
          (db:with-transaction ()
            (destructuring-bind (title text) (multipart-field-values "title" "text")
              (setf (post-title post) title
                    (post-text post) (remove-carriage-return text))))
          (http-redirect (path-to post)))
        (http-err 501 "Unauthorized"))
    (http-err 404 "Not Found")))
