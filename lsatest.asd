(asdf:defsystem #:lsatest
  :description "DHCP client and server unit testing code"
  :author "gus"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (
	       #:alexandria
	       #:serapeum
	       #:flexi-streams
	       #:usocket
	       #:swank
	       #:closer-mop	       
	       #:uiop
	       #:lparallel   
	       #:cl-ppcre
	       #:fiasco
	       #:trivia
	       #:trivia.ppcre
	       #:numex
	       #:lsa
               #:cl-ppcre
	       )
  :components ((:file "lsatest-package")
	       (:file "lsa-ut")
	       ))

