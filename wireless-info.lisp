(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defparameter *iw-dev-scanner-splitter* (ppcre:create-scanner "^phy#\\d+:" :multi-line-mode t))

(defvar *wifi-info-buffer* nil)

(defun iw-list-raw ()
  "memoize the text output from running the iw list linux command"
  (unless
      *wifi-info-buffer*
    (setf *wifi-info-buffer*
	  #+nil(inferior-shell:run/s "iw list")
	  (let ((p1 (eazy-process:shell `("iw" "list"))))
	    (with-output-to-string (output)
	      (with-open-file (s (eazy-process:fd-as-pathname p1 1))
		(uiop:copy-stream-to-stream s output)))
	    )))
  *wifi-info-buffer*
  )

(defun iw-dev-raw ()
  "return the results of 'iw dev' as a single string"
  (let ((p1 (eazy-process:shell `("iw" "dev"))))
    (with-output-to-string (output)
      (with-open-file (s (eazy-process:fd-as-pathname p1 1))
	(uiop:copy-stream-to-stream s output)))
    )
  )

(defun get-temp-file ()
  (let* ((i (random  1000000))
	 (fname (format nil "~a~a" (uiop:temporary-directory) i) ))
    (if (probe-file fname)
	(get-temp-file)
	fname)))

(cffi:defcfun "waitpid" :int (pid :int) (int :pointer))



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
  "process the linux iw list command, putt"
  (nth-value 1 (chomp-and-count line))
  )

(defun tree-walker (tree)
  (optima:match
      tree
    (() '())
    ;; capabilities
    ((cons (and (type string)
		(optima.ppcre:ppcre #?"Capabilities:\\s+0x([0-9a-fA-F]+)" n)) rest)
     (let ((num (parse-integer n :radix 16)))
       (cons :capabilities (cons num (tree-walker rest)))))
    ((cons (and (type string)
		(optima.ppcre:ppcre #?"Band (\\d+)" n)) rest)
     (cons :band (cons (parse-integer n) (tree-walker rest))))
    ;; physical dev
    ((cons (and (type string)
		(optima.ppcre:ppcre "Wiphy phy(\\d+).*" n)) rest)
     (let ((num (parse-integer n)))
       (cons (list :phy num) (tree-walker rest))))
    ;; mode
    ((cons (and (type string)
		(optima.ppcre:ppcre "Supported interface modes:")
		) rest)
     (cons :modes (tree-walker rest)))
    ((type string)
     (chomp-and-count tree))
    ((cons car cdr)
     (cons (tree-walker car)
	   (tree-walker cdr))))
  )

(defun iw-dev-tree-walker (tree)
  (optima:match
      tree
    (() '())
    ((cons (and (type string)
		(optima.ppcre:ppcre "phy#(\\d+)" n))
	   rest)
     (let ((num (parse-integer n)))
       (cons :phy (cons num (iw-dev-tree-walker rest)))))
    ((cons (and (type string)
		(optima.ppcre:ppcre "Interface (\\w+)" iface))
	   rest)
     (cons :iface (cons iface (iw-dev-tree-walker rest))))
    ((type string)
     (chomp-and-count tree))
    ((cons car cdr)
     (cons (iw-dev-tree-walker car)
	   (iw-dev-tree-walker cdr))))
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

#+nil(defun wsparse (buff)
  "Turn a block of text into a tree"
  (loop :for l :in (ppcre:split "(\\n|\\r)" buff)
     :collect (multiple-value-list (chomp-and-count l)))
  )

(defun iw-list-tree (&key (txt (iw-list-raw)))
  (let* ((seq (split-into-lines txt)))
    (tree-walker (seq->tree seq :level-key #'iw-list-level :level-init 0))
    ))

(defun iw-dev-tree (&key (txt (iw-dev-raw)))
  (let* ((seq (split-into-lines txt)))
    (iw-dev-tree-walker (seq->tree seq :level-key #'iw-list-level :level-init 0))
    ))

(defun get-dev (dev-lst &key key )
  "get device-tree by number, if key is nil return the numeric
device-ids of the system (linux)"
  (unless dev-lst
    (setf dev-lst (iw-list-tree)))
  (cond
    ((null key)
     (serapeum:filter-map (optima.extra:lambda-match
			    ((list :phy n) n))
			  dev-lst))
    ((numberp key)
     (loop :for (A tree) :on dev-lst :by #'cddr :do
	  (match
	      A
	    ((list :phy n)
	     ;;(format t "~a,key=~a,number? ~a~%" n  key (equal key n))
	     (when (and (numberp n)
			(equal n key))
	       (return-from get-dev tree)))))
     nil)
    )
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

(defun phy-supports-monitor? (n)
  (let ((tree (get-dev nil :key n)))
    (loop :for n :on tree :do
	 (optima:match
	     n
	   ((list* :modes  (and (type list)
				mlst) _)
	    (return-from phy-supports-monitor? (member "monitor" mlst :test #'equal))))
	 )
    nil))

(defun extract-band-info (dev-tree)
  "returns a list of subtrees, one for each band"
  ;; :band num subtree
  (serapeum:collecting
    (loop :for p :on dev-tree :do
	 (optima:match
	     p
	   ((list* :band (and (type number)
			      num)
		   (and (type list)
			subtree)
		   _)
	    (collect (cons num subtree)))))
    )
  )

(defun extract-capabilities-from-band-info (band-info)
  (loop :for p :on band-info :do
       (optima:match
	   p
	 ((list* :capabilities
		 (type number)
		 (and (type list)
		      cap-list)
		 _)
	  (return-from extract-capabilities-from-band-info cap-list))))
  nil)

(defun phys-id-supports-dsss-cck-40? (phys-id)
  (let ((tree (get-dev nil :key phys-id)))
    (member "DSSS/CCK HT40"
	    (remove-duplicates 
	     (apply #'append (mapcar #'extract-capabilities-from-band-info
				     (extract-band-info tree)))
	     :test #'equal
	     )
	    )
    )
  )

(defun ifname->dev-num (ifname)
  "maps the interface name to a wifi device-number"
  (let ((iw-dev-tree (iw-dev-tree))
	(phy-num nil))
    (loop :for p :on iw-dev-tree :do
	 (optima:match
	     p
	   ((list* :phy (and (type number)
			     num) _)
	    (setf phy-num num))
	   ((list* (list* :iface (and (type string)
				      ifn) _) _)
	    (if (equal ifn ifname)
		(return-from ifname->dev-num phy-num))))
	 )
    )
  nil
  )

(defun ensure-monitor!! ()
  "Create a monitor interface on each of the AP links.  We currently
brute-force each of the wireless phy interfaces."
  (unless (monitor-exists?)
    (loop :for n :in (phys-iota) :do
	 (when (phy-supports-monitor? n)
	   (let ((cmd (setup-monitor-command n)))
	     (inferior-shell:run cmd :on-error nil)
	     )
	   )
	 )
    )
  )

(defun hostapd-file ()
  "/etc/hostapd/hostapd.conf")


(defun setup-hostapd (&key ifname ssid channel pw)
  ;; map ifname to dev-number
  (serapeum:and-let*
      ( ;; bad hack to move things along
       (filename (hostapd-file))
       (pathname (pathname filename))
       )
    (uiop:ensure-all-directories-exist (list pathname))
    (with-open-file
	(out  pathname
	      :direction :output
	      ;;:element-type :utf-8 ;;'(unsigned-byte 8)
	      :if-exists :supersede
	      :if-does-not-exist :create)
      (princ
       (lsa:hostapd ifname
		    ssid
		    pw
		    :channel  channel
		    :dsss-cck-40 (ifname->dev-num ifname)
		    )
       out)
      )
    )
  )
