;;;; lisp-linux-sa-utils.asd

(asdf:defsystem #:lsa
  :description "Describe lisp-linux-sa-utils here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (
	       #:alexandria
	       #:cffi
	       #:cl-interpol
	       #:cl-ppcre
	       #:fare-memoization
	       #:iolib
	       #:iterate
	       #:log4cl
	       #:numex
	       #:osicat
	       #:rutils
	       #:serapeum
	       #:stringhere
	       #:trivia
	       #:trivia.ppcre
	       #:trivial-backtrace
	       #:uiop
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
