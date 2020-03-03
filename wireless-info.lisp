
(in-package #:lsa)


(defun iw-dev-raw ()
  "return the results of 'iw dev' as a single string"
  (inferior-shell:run/s "iw dev")))


(defun iw-dev-simple ()
  "Return 'iw dev' as a (list (phys  (interface ...) (interface ..))
                              ...)"
  (let (rlst) ;; record list
    (loop :for e in (ppcre:split "(\\r|\\n)" *iw-dev*) 
       :do
	 (if (alphanumericp (elt e 0))
	     (push (list e) rlst)
	     (push e (car rlst))))
    (reverse (mapcar #'reverse rlst)))

