;;;; lisp-linux-sa-utils.asd

(asdf:defsystem #:lsa
  :description "Describe lisp-linux-sa-utils here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (
	       #:uiop
	       #:trivia
	       #:trivia.ppcre
	       #:inferior-shell
	       #:stringhere
	       )
  :components ((:file "package")
               (:file "lisp-linux-sa-utils"))
  )
