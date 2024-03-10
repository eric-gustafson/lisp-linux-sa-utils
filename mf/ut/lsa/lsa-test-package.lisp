(fiasco:define-test-package
	      #:lsa-test
	    (:local-nicknames 
	     (:al  :alexandria)
	     (:a :alexandria)
	     (:re :ppcre)
	     (:i :iterate)
	     (:s :serapeum)
	     (:r :rutils)
	     )	    
	    (:use #:cl #:lsa #:trivia)
	    )
	  
