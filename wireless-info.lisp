(in-package #:lsa)

(defparameter *iw-dev-scanner-splitter* (ppcre:create-scanner "^phy#\\d+:" :multi-line-mode t))

(defun iw-dev-raw ()
  "return the results of 'iw dev' as a single string"
  (inferior-shell:run/s "iw dev")
  )

(defun iw-dev-simple ()
  "Return 'iw dev' as a (list (phys  (interface ...) (interface ..))
                              ...)"
  (ppcre:split "(\\n|\\r)" (iw-dev-raw))
  )

(defun iw-dev-split-phy*-buff (txt)
  (ppcre:split *iw-dev-scanner-splitter* txt))

(defun iw-dev->tree (txt)
  (let ((tree (make-instance 'tree))
	phy)
    (loop :for l :in (ppcre:split "(\\n|\\r)" txt) :do
	 (trivia:match
	     l
	   ((trivia.ppcre:ppcre "^phy#\\d+.*")
	    (setf phy (chomp-and-count l))
	    (cd! tree  `(,phy) :if-not-exist :create))
	   ((trivia.ppcre:ppcre "Interface\\s+(.+)" iface-name)
	    (cd! tree `(,phy ,iface-name) :if-not-exist :create))
	   ((trivia.ppcre:ppcre "\\s+(\\w+)\\s+(.*)" w stuff)
	    (add! tree (intern (string-upcase w) :keyword) stuff))
	   )
	 )
    (root tree)
    )
  )

(defun chomp-and-count (str)
  "Returns two values, the string trimmed on the LHS, along with count of whitespace removed"
  (let ((n (length str)))
    (let ((i (loop :for i :from 0 :upto n
		:while (not (alphanumericp (elt str i)))
		:finally (return i))))
      (values (subseq str i)  i))
    )
  )

(defun wsparse (buff)
  "Turn a block of text into a tree"
  (loop :for l :in (ppcre:split "(\\n|\\r)" buff)
     :collect (multiple-value-list (chomp-and-count l)))
  
  )

(defun monitor-exists? ()
  "returns a list of the monitor links, from the ip-link function"
  (remove-if-not (lambda(obj)
		   (search "mon" (car obj)))
		 (ip-link))
  )


(defun phys-iota (&key (txt (iw-dev-raw)))
  "Returns a list of integers for each of the physical-wireless interfaces"
  (serapeum:collecting
    (loop :for l :in (ppcre:split "(\\n|\\r)" txt) :do
	 (trivia:match
	     l
	   ((trivia.ppcre:ppcre "^phy#(\\d+).*" n)
	    (collect (parse-number n))))
	 ))
  )

;; Note, not sure how to target when we have more than 1 interface, but since
;; I don't know why trying to setup monitoring would ever be bad, we just do it
;; for all.  Kind of a broad-spectrum approach.

(defun ensure-monitor!! ()
  "Create a monitor interface on each of the AP links.  We currently brute-force each of the wireless phy interfaces."
  (unless (monitor-exists?)
    (loop :for n :in (phys-iota) :do
	 (let ((cmd (format nil "iw phy phy~a interface add mon~a type monitor && ifconfig mon~a up"
			    n n n)))
	   (inferior-shell:run  cmd)
	   )
	 )
    )
  )
