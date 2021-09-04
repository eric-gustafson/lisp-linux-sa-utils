;;;; lisp-linux-sa-utils.asd

(asdf:defsystem #:lsa
  :description "Describe lisp-linux-sa-utils here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (
	       #:serapeum
	       #:cl-interpol
	       #:uiop
	       #:trivia
	       #:trivia.ppcre
	       #:optima
	       #:optima.ppcre
	       #:fare-quasiquote-optima
	       #:fare-quasiquote-readtable
	       #:stringhere
	       #:numex
	       #:cffi
	       #:iolib
	       #:uiop-shell
	       )
  :components ((:file "package")
               (:file "lsa")
	       (:file "routing")
	       (:file "wireless-info")
	       (:file "hostapd")
	       (:file "git")
	       (:file "users")
	       )
  )
