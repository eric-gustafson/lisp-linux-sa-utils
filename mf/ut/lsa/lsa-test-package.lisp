(fiasco:define-test-package
	      #:lsa-test
	    (:local-nicknames 
	     (:al  :alexandria)
	     (:a :alexandria)
	     (:pm :optima)
	     (:pme :optima.extra)
	     (:re :ppcre)
	     (:pm.re :optima.ppcre)
	     (:s :serapeum)
	     (:r :rutils)
	     )	    
	    (:use #:cl #:lsa
		  #:serapeum)
	    )
	  