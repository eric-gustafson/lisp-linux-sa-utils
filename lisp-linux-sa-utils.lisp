;;;; lisp-linux-sa-utils.lisp

(in-package #:lsa)

(defclass link ()
  (
   (name :accessor name :initarg :name :initform "")
   (mtu  :accessor mtu :initarg :mtu :initform "")
   (qdisk :accessor qdisk :initarg :qdisk :initform "")
   (state :accessor state :initarg :state :initform  "")
   (mode :accessor mode :initarg :mode :initform "")
   (group :accessor group :initarg :group :initform "")
   (mac :accessor mac :initarg :mac :initform "")
   (ltype :accessor ltype :initarg :ltype :initform "")
   (broadcast :accessor broadcast :initarg :broadcast :initform "")
   ;; (:accessor :initarg :initform)
   )
  )

(defclass ip-addr (link)
  (
   (addr :accessor addr :initarg :addr :initform nil)
   )
  )

(defparameter *extractor-methods* '())

(defclass host ()
  (
   (iflst :accessor iflst :initarg :iflst :initform '())
   )
  )

(defmacro def-if-pattern-extractor ((objvar) &body body)
  (let ((lstvar (gensym)))
    `(push #(lambda(,objvar ,lstvar)
	     (trivia:match
		 ,lstvar
	       ,@body
	       ))
	   *extractor-methods*)
    ))

(def-if-pattern-extractor (obj)
    ((list _ ifstr _ "mtu")
     (setf (name obj) ifstr))
  )

(defmethod fill-object ((obj link) lst)
  (loop :for e :on lst :do
     (loop :for emethod in *extractor-methods* :do
	(funcall emothd obj e))
     )
  )

(defmethod print-object ((obj link) stream)
  (print-unreadable-object
      (obj stream :type t)
    (with-slots
	  (name mtu qdisk state mode group mac ltype broadcast)
	obj
      (format stream "~a,~a,~a,state=~a,mode=~a,group=~a,~a,~a,~a"
	      name mtu qdisk state mode group
	      mac ltype broadcast
	      ))
    )
  )


(defmethod print-object ((obj ip-addr) stream)
  (print-unreadable-object
      (obj stream :type t)
    (with-slots
	  (name mtu qdisk state mode group mac ltype broadcast addr)
	obj
      (format stream "~a,~a,~a,~a,state=~a,mode=~a,group=~a,~a,~a,~a"
	      name addr mtu qdisk state mode group
	      mac ltype broadcast
	      ))
    )
  )

(defun common-splitter (txt)
  "splits up sections of code from ip, such as 'ip addr' and 'ip link'.  Returns a ((lo ...) (eth0 ...)) wher everything is a string."
  (trivia:match
      (ppcre:split *if-scanner-splitter*  txt)
    ((list* "" rest)
     (mapcar #'(lambda(str)
		 (let ((stuff (ppcre:split "\\s+" str)))
		   (trivia:match
		       stuff
		     ((list* "" if rest)
		      (trivia:match
			  if
			((trivia.ppcre:ppcre "(.+):" x)
			 (cons x rest))
			(otherwise
			 (cons if rest))))
		     (otherwise
		      stuff))))
	     rest)
     )
    )
  )
  
(defun ip-link ()
  (common-splitter (inferior-shell:run/s "/sbin/ip link"))
  )

(defun ip-addr ()
  (common-splitter (inferior-shell:run/s "/sbin/ip addr")))

(defun ip-addr-objs ()
  (serapeum:filter-map
   (trivia:lambda-match
     ((list*
       if _
       "mtu" _
       "qdisc" _
       "state" _
       "group" _
       "qlen" _
       type mac
       "brd" _
       "inet" ip/cidr
       rest)
      (make-instance 'ip-addr
		     :name if
		     :addr ip/cidr)))
   (ip-addr)))

(defun ip-link-objs ()
  (serapeum:filter-map
   #'(lambda(obj)
	      (trivia:cmatch
		  obj
		((list* name thing 
			"mtu" mtu
			"qdisc" qd
			"state" state
			"mode" mode
		        "group" group
			"qlen" qlen
			type
			mac
			"brd"
			brd			
			rest)
		 (make-instance 'link
				:name name
				:mtu (parse-integer mtu)
				:qdisk qd
				:state state
				:mode mode
				:group group
				:mac mac
				:ltype type
				:broadcast brd
				))))
	  (ip-link)))
	    
(defparameter *if-scanner-splitter* (ppcre:create-scanner "^\\d+:" :multi-line-mode t))
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
	 ;;(format t "~a,~a~%" ifstr off)
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
  
(defun iwconfig-interface-list ()
  (let ((results '()))
    (loop :for line :in (inferior-shell:run/lines "iwconfig")
       :do
       (trivia:multiple-value-match
	   (ppcre:scan-to-strings "^(\\w+)\\s.*" line)
	 ((_ #(first))
	  (push first results))))
    results))

  
       
(progn
  (stringhere:enable-txt-syntax)	  

 
(defun hostapd (iface ssid passphrase)
  (declare (type (string iface)))
  (let ((output
	 (with-output-to-string (*standard-output*)
#{	   
### FILE: hostapd.conf
### Wireless network name ###                                                                                                                          

interface=,(princ iface)
driver=nl80211
country_code=US
ssid=,(princ ssid)
hw_mode=g
channel=1
wpa=2
wpa_passphrase=,(princ passphrase)
## Key management algorithms ##                                                                                                                        
wpa_key_mgmt=WPA-PSK

## Set cipher suites (encryption algorithms) ##                                                                                                        
## TKIP = Temporal Key Integrity Protocol                                                                                                              
## CCMP = AES in Counter mode with CBC-MAC                                                                                                             
wpa_pairwise=TKIP
rsn_pairwise=CCMP

## Shared Key Authentication ##                                                                                                                        
auth_algs=1

## Accept all MAC address ###                                                                                                                          
macaddr_acl=0
}
)))
    output)
  )


(stringhere:disable-txt-syntax)
)
