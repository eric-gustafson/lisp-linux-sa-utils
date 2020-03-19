(in-package #:lsa)

(defparameter *iw-dev-scanner-splitter* (ppcre:create-scanner "^phy#\\d+:" :multi-line-mode t))

(defvar *wifi-info-buffer* nil)

(defun wifi-info ()
  (unless
      *wifi-info-buffer*
    (setf *wifi-info-buffer* (inferior-shell:run/s "iw list")))
  *wifi-info-buffer*
  )

(defun iw-dev-raw ()
  "return the results of 'iw dev' as a single string"
  (inferior-shell:run/s "iw dev")
  )

(defun update-queue (stack-o-queues e level-key)
  ;; returns true if we could update the stack-queue with the value
  (destructuring-bind (stack-lev stack-q)
      (car stack-o-queues)
    (let ((elev (funcall level-key e)))
      (cond
	((> elev stack-lev) 
	 (push (list elev (serapeum:queue e)) stack-o-queues)
	 )
	((= elev stack-lev)
	 (serapeum:enq e stack-q)
	 stack-o-queues
	 )
	)
      )
    )
  )

(defun pop-and-combine (squeue)
  "Take the queue off of the top of the stack, and convert it to a
list and append it to the queue at what is the new top of the stack"
  (let* ((old (pop squeue))
	 (new (car squeue))
	 (oq (cadr old))
	 (nq (cadr new)))
    (serapeum:enq (serapeum:qlist oq) nq)
    squeue
    ))

(defun seq->tree (seq &key (level-key #'car) (level-init 1))
  "Takes a sequence and builds a tree from it.  It rebuilds a
  depth-first tree traversal.  The level-key function gets the level
  that the element belongs to."
  (let (
	(level-stack-queue (list (list level-init (serapeum:queue)))) ;; A stack of queues
	)
    (map 'list
	 #'(lambda(e)
	     (loop :do
		  (let ((stack (update-queue level-stack-queue e level-key)))
		    (when stack
		      (setf level-stack-queue stack)
		      (loop-finish))
		    (setf level-stack-queue (pop-and-combine level-stack-queue))
		    )
		  ))
	 seq
	 )
    (loop :while (> (length level-stack-queue) 1) :do
	 (setf level-stack-queue (pop-and-combine level-stack-queue))
	 )
    (apply #'append
	   (mapcar (lambda(obj)
		     (serapeum:qlist (cadr obj)))
		   level-stack-queue))
    )
  )

(defun split-into-lines (txt)
  (ppcre:split "(\\n|\\r)" txt))
  

(defun iw-dev-simple ()
  "Return 'iw dev' as a (list (phys  (interface ...) (interface ..))
                              ...)"
  (split-into-lines (iw-dev-raw))
  )

(defun iw-dev-split-phy*-buff (txt)
  (ppcre:split *iw-dev-scanner-splitter* txt))

;; TODO Get this into a tree so that we can than reduce
;;  using patterns and other high level constructs
(defun iw-list-level (line)
  (nth-value 1 (chomp-and-count line))
  )

(defun tree-walker (tree)
  (optima:match
   tree
   (() '())
   ((cons (and (type string)
	       (optima.ppcre:ppcre "Wiphy phy(\\d+).*" n)) rest)
    (let ((num (parse-integer n)))
      (cons :phy (cons num (tree-walker rest)))))
   ((type string)
    (chomp-and-count tree))
   ((cons car cdr)
    (cons (tree-walker car)
	  (tree-walker cdr))))
  )

(defun iw-list->tree (&key (txt (wifi-info)))
  (let* ((seq (split-into-lines txt)))
    (seq->tree seq :level-key #'iw-list-level :level-init 0)
    ))

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
(defun setup-monitor-command (phys-id)
  (format nil "iw phy phy~a interface add mon~a type monitor && ifconfig mon~a up"
	  phys-id phys-id phys-id))
  
(defun ensure-monitor!! ()
  "Create a monitor interface on each of the AP links.  We currently brute-force each of the wireless phy interfaces."
  (unless (monitor-exists?)
    (loop :for n :in (phys-iota) :do
	 (let ((cmd ))
	   (inferior-shell:run  cmd)
	   )
	 )
    )
  )
