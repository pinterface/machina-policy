(asdf:defsystem #:robots.txt
  :version "0"
  :description "Parser for robots.txt files"
  :author "pinterface <pix@kepibu.org>"
  :licence "MIT"
  :depends-on (#:alexandria #:cl-ppcre #:puri)
  :serial t
  :components ((:file "package")
               (:file "wild-uris")
               (:file "parser")
               (:file "api")))
