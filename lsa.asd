;;;; lisp-linux-sa-utils.asd

(asdf:defsystem #:lsa
  :description "Describe lisp-linux-sa-utils here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (
	       #:alexandria
	       #:cl-interpol
	       #:serapeum
	       #:cl-ppcre
	       #:uiop
	       #:trivia
	       #:trivia.ppcre
	       #:optima
	       #:optima.ppcre
	       #:stringhere
	       #:numex
	       #:cffi
	       ;;#:iolib
	       #:uiop-shell
	       #:rutils
	       )
  :components ((:file "package")
               (:file "lsa")
	       (:file "routing")
	       (:file "wireless-info")
	       (:file "hostapd")
	       (:file "git")
	       ;;(:file "users")
	       )
  )
