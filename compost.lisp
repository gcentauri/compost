;;;; compost.lisp

(in-package #:compost)

(defparameter +config-file+
  "compost.conf")

(defvar *root-url* nil)

(defun config-file-path ()
  (merge-pathnames +config-file+ (user-homedir-pathname)))

(defun load-initial-users ()
  (unless (uiop:file-exists-p (config-file-path))
    (error "Cannot load initial users: cannot find config"))
  (let ((config
          (with-open-file (input (config-file-path))
            (read input))))
    (loop :for (username password) :in (getf config :users)
          :do (make-user username password))))

(defun start (&key (port 5000))
  (unless (boundp 'db:*store*)
    (initialize-datastore))

  (when (zerop (length (all-users)))
    (load-initial-users))

  (initialize-tag-index)
  (initialize-invite-tab)

  (lzb:start :port port))

(defun help-menu ()
  (format t "Enter one of the following commands:~%~{~s~%~}~%"
          '(:quit :snapshot))
  (force-output))

(defun start-loop () 
  (let ((port (parse-integer (first (uiop:command-line-arguments))))
        (root-url (second (uiop:command-line-arguments))))
    (assert (https-url-p root-url))
    (setf *root-url* root-url)
    (bt:make-thread (lambda () (swank:create-server :port 4006 :dont-close t)))
    (start :port port)
    (help-menu)
    (loop :for command = (read)
          :do (case command
                (:quit
                 (format t "quitting~%")
                 (lzb:stop)
                 (uiop:quit))
                (:snapshot
                 (format t "snapshotting~%")
                 (db:snapshot))
                (t
                 (help-menu))))))
