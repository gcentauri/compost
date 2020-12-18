;;;; compost.lisp

(in-package #:compost)

(defun start (&key (port 5000))
  (unless (boundp 'db:*store*)
    (initialize-datastore))
  (lzb:start :port port))




