;;;; lisp-linux-sa-utils.asd

(asdf:defsystem #:lsa
  :description "Describe lisp-linux-sa-utils here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (
	       #:serapeum
	       #:uiop
	       #:trivia
	       #:trivia.ppcre
	       #:inferior-shell
	       #:stringhere
	       #:numex
	       )
  :components ((:file "package")
               (:file "lisp-linux-sa-utils"))
  )
