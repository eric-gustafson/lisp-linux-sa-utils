(asdf:defsystem #:lsa-test

	       :description "Describe lisp-linux-sa-utils here"
	       :depends-on  ("alexandria" "cl-interpol" "serapeum" "cl-ppcre" "uiop" "trivia" "trivia.ppcre" "optima" "optima.ppcre" "stringhere" "numex" "cffi" "iolib" "uiop-shell" "rutils" "fare-memoization" :LSA)
	       :components ((:file "lsa-test-package")
			    (:file "usr/ut")
				 
			    )
       )
     