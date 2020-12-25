;;;; compost.asd

(asdf:defsystem #:compost
  :description "A message board for small groups"
  :author "Colin Okay <okay@toyful.space>"
  :license  "AGPL-3.0"
  :version "0.1.0"
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
               #:markdown.cl
               #:swank)
  :components ((:file "package")
               (:file "util")
               (:file "model")
               (:file "views")
               (:file "routes")
               (:file "compost")))

