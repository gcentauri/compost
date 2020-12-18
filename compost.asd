;;;; compost.asd

(asdf:defsystem #:compost
  :description "Describe compost here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:bknr.datastore
               #:lazybones
               #:spinneret
               #:parenscript
               #:lass
               #:local-time
               #:ironclad
               #:cl-base64
               #:jonathan
               #:markdown.cl)
  :components ((:file "package")
               (:file "util")
               (:file "model")
               (:file "views")
               (:file "routes")
               (:file "compost")))

