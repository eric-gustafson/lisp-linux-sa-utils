(asdf:defsystem lsa-test
		:description "Describe lisp-linux-sa-utils here"
		:depends-on  ("serapeum" "cl-ppcre" "cl-interpol" "uiop" "trivia" "trivia.ppcre" "optima" "optima.ppcre" "fare-quasiquote-optima" "fare-quasiquote-readtable" "stringhere" "numex" "cffi" "iolib" "uiop-shell" LSA)
		:components ((:file "lsa-test-package")
					 #+nil(:file "lsa-test-ut")
					 (:file "usr/ut")
						  
					 )
		)
	  