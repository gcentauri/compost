;;;; model.lisp
 
(in-package #:compost)


;;; INITIALIZATION

(defparameter +data-store-directory-name+
  "compost-store")

(defun data-store-path ()
  (make-pathname
   :directory (append (pathname-directory (user-homedir-pathname))
                      (list +data-store-directory-name+))))

(defun initialize-datastore ()
  (ensure-directories-exist (data-store-path))
  (make-instance 'db:mp-store
                 :directory (data-store-path)
                 :subsystems (list (make-instance 'db:store-object-subsystem)
                                   (make-instance 'db:blob-subsystem))))

;;; CLASSES

(defclass topic (db:store-object)
  ((name
    :accessor topic-name
    :initarg :name
    :initform (error "topics must have a name")
    :index-type bknr.indices:string-unique-index
    :index-reader topic-by-name
    :index-values all-topics))
  (:metaclass db:persistent-class))

(defclass user (db:store-object)
  ((name
    :accessor user-name
    :initarg :name
    :initform (error "USERs must have a name")
    :index-type bknr.indices:string-unique-index 
    :index-reader user-by-name
    :index-values all-users)
   (pw-hash
    :accessor pw-hash
    :initarg :pw
    :initform (error "USERs must have a password")))
  (:metaclass db:persistent-class ))

(defclass session (db:store-object)
  ((user
    :reader session-user
    :initarg :user)
   (cookie
    :reader session-cookie
    :initarg :cookie
    :index-type bknr.indices:string-unique-index
    :index-reader session-by-cookie))
  (:metaclass db:persistent-class))

(defclass post (db:store-object)
  ((created
    :reader post-created
    :initform (get-universal-time))
   (text
    :accessor post-text
    :initarg :text
    :initform "")
   (render-style
    :accessor post-render-style
    :initarg :render-style
    :initform 'identity
    :documentation "Name of a function used to render text for display")
   (user
    :reader post-user
    :initarg :user
    :initform (error "Posts must have a user")
    :index-type bknr.indices:hash-index
    :index-reader posts-by-user))
  (:metaclass db:persistent-class))

(defclass attachment (db:blob)
  ((filename
    :reader attachment-filename
    :initarg :filename
    :initform (error "Attachments must have a name"))
   (post
    :reader attachment-post
    :initarg :post
    :initform (error "Attachments must have a post")
    :index-type bknr.indices:hash-index
    :index-reader post-attachments))
  (:metaclass db:persistent-class))

(defclass title-post (post)
  ((title
    :accessor post-title
    :initarg :title
    :initform (error "Title posts must have a title"))
   (topic
    :accessor post-topic
    :initarg :topic
    :initform nil
    :index-type bknr.indices:hash-index
    :index-reader title-posts-by-topic))
  (:metaclass db:persistent-class))

(defclass reply-post (post)
  ((predecessor
    :accessor reply-to
    :initarg :predecessor
    :initform (error "reply posts require a predecessor")
    :index-type bknr.indices:hash-index
    :index-reader replies-to))
  (:metaclass db:persistent-class))


;;; MODEL FUNCTIONS

(defun set-post-style (post style)
  "STYLE is one of :TEXT or :MD"
  (setf (post-render-style post)
        (ecase style
          (:text 'identity)
          (:md   'markdown:parse))))

