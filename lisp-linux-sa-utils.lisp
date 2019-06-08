;;;; lisp-linux-sa-utils.lisp

(in-package #:lsa)

(defun ip-link ()
  (let ((txt (inferior-shell:run/lines "/sbin/ip link"))
	)
    (loop :for results in
       (loop
	  :for i from 1
	  :for l in txt
	  :when (oddp i) :collect (cdr (ppcre:split "\\s" l)))
       :collect
       (trivia:match
	   results
	 ((cons first rest)
	  (cons (ppcre:regex-replace ":$" first "") rest))))
    )
  )

(defparameter *if-scanner* (ppcre:create-scanner "^\\d+:\\s+([^:]+):" :multi-line-mode t))
(defparameter *ip-addr-scanner* (ppcre:create-scanner ".*inet\\s+(\\d+\\.\\d+\\.\\d+\\.\\d+)/(\\d+)" :multi-line-mode t))
(defparameter *mac-addr-scanner* (ppcre:create-scanner ".*link/\\w+\\s+(\\S+)" :multi-line-mode t))

(defun extract-if (buff &key (start 0))
  "returns 2 values, the interface and the offset into the buffer or nil if nothing found"
  (trivia:multiple-value-match
      (ppcre:scan *if-scanner* buff :start start)
    ((_ mend rsv rev)
     (if (numberp mend)
	 (values (subseq buff (elt rsv 0) (elt rev 0))
		 mend))
     )
    )
  )

(defun extract-ip (buff &key (start 0))
  (trivia:multiple-value-match
      (ppcre:scan *ip-addr-scanner* buff :start start)
    ((mstart mend rsv rev)
     (if (numberp mstart)
	 (values mend
		 (subseq buff (elt rsv 0) (elt rev 0))
		 (parse-integer (subseq buff (elt rsv 1) (elt rev 1)))
		 )))
    )
  )

(defun extract-mac (buff &key (start 0))
  (trivia:multiple-value-match
      (ppcre:scan *mac-addr-scanner* buff :start start)
    ((mstart mend rsv rev)
     ;;(format t "~a ~a ~a ~a~%" mstart mend rsv rev)
     (if (numberp mstart)
	 (values (subseq buff (elt rsv 0) (elt rev 0))
		 mend)))
    )
  )

(defun extract-ip-record (enq txt &key (offset 0))
  "Add's interface to the queue"
  (let ((q (serapeum:queue))
	(%offset offset))
    (labels ((capture (obj off)
	       (serapeum:enq obj q)
	       (setf %offset off)
	       ))
      (trivia:match
	  (multiple-value-list (extract-if txt :start offset))
	((list ifstr off)
	 (format t "~a,~a~%" ifstr off)
	 (unless off (trivia.fail:fail))
	 (capture ifstr off)
	 ;; Add more stuff if we can find it.
	 (trivia:match
	     (multiple-value-list (extract-mac txt :start offset))
	   ((list mac offset)
	    (unless offset (trivia.fail:fail))
	    (capture mac offset)
	    ))
	 (trivia:match
	     (multiple-value-list (extract-ip txt :start offset))
	   ((list offset ip cidr)
	    (unless offset (trivia.fail:fail))
	    (capture (list ip cidr) offset)
	    )
	   )
	 ;;We got an interface object, add it to the whole and
	 ;; return the offset
	 (funcall enq (serapeum:qlist q))
	 %offset)
	(otherwise
	 nil)
	))
    )
  )
  
(defun ip-addr ()
  "return an associate list of (interface  ip)"
  (let* ((txt (inferior-shell:run/s "/sbin/ip addr"))
	 (q (serapeum:queue))
	 (offset 0))
    (labels ((adder (obj)
	       (serapeum:enq obj q)))
      (loop :do
	 (progn
	   (format t "wtf:~a~%" offset)
	   (setf offset (extract-ip-record #'adder txt :offset offset))
	   (format t "~a~%" offset)
	   (unless offset (loop-finish)))))
    (serapeum:qlist q)
    ))

(defun iwconfig-interface-list ()
  (let ((results '()))
    (loop :for line :in (inferior-shell:run/lines "iwconfig")
       :do
       (trivia:multiple-value-match
	   (ppcre:scan-to-strings "^(\\w+)\\s.*" line)
	 ((_ #(first))
	  (push first results))))
    results))

  
       
	  
