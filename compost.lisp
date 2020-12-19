;;;; compost.lisp

(in-package #:compost)

(defun start (&key (port 5000))
  (local-time:reread-timezone-repository )
  (unless (boundp 'db:*store*)
    (initialize-datastore))
  (lzb:start :port port))




