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
  (let ((tree (make-instance 'tree)))
    (loop :for l :in (ppcre:split "(\\n|\\r)" txt) :do
	 (trivia:match
	     l
	   ((trivia.ppcre:ppcre "^phy#(\\d+).*" phy-n)
	    (cd! tree  `(,(chomp-and-count l)) :if-not-exist :create))
	   ((trivia.ppcre:ppcre "^Interface\\s+(.+)" iface-name)
	    (cd! tree iface-name :if-not-exist :create))
	   ((trivia.ppcre:ppcre "^\\s+(\\w+)\\s+(.*)" w stuff)
	    (add! tree (intern w :keyword) stuff))
	   )
	 )
    tree
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

(defun get-monitor-phy-target ()
  "returns phy0, phy1 ... based on an inspection of the underlying O.S."
  "phy0")

(defun ensure-monitor!! ()
  "Create a monitor interface on each of the AP links."
  (unless (monitor-exists?)
    (let ((cmd (format nil "iw phy ~a interface add mon0 type monitor && ifconfig mon0 up"
		       (get-monitor-phy-target))))
      (inferior-shell:run  cmd)
      )
    )
  )
