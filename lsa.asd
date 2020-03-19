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
	       #:optima
	       #:optima.ppcre
	       #:inferior-shell
	       #:stringhere
	       #:numex
	       #:cffi
	       )
  :components ((:file "package")
               (:file "lsa")
	       (:file "wireless-info")
	       (:file "hostapd")
	       (:file "tree")
	       )
  )
