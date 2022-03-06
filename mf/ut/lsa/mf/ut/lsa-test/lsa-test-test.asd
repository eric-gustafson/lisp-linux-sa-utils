(asdf:defsystem #:lsa-test-test

	       :description "Describe lisp-linux-sa-utils here"
	       :depends-on  ("alexandria" "cl-interpol" "serapeum" "cl-ppcre" "uiop" "trivia" "trivia.ppcre" "optima" "optima.ppcre" "stringhere" "numex" "cffi" "uiop-shell" "rutils" "lsa" :LSA-TEST)
	       :components ((:file "lsa-test-test-package")
			    (:file "usr/ut")
				 
			    )
       )
     