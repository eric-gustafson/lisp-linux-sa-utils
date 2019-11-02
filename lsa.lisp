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

(defvar *vlan-id* 0)

(defclass vlan ()
  (
   (id :accessor id :initarg :id)
   (dev :accessor dev :initarg dev)
   )
  )

(defun add-vlan (pif ip cidr-block)
  (unless (and ip cidr-block)
    (error "add-vlan - must supply ip and cidr-block"))
  (handler-case
      (progn
	(incf *vlan-id*)
	(inferior-shell:run (format nil "/sbin/ip link add link ~a name ~a.~a type vlan id ~a" pif pif *vlan-id*  *vlan-id*) :on-error nil)
	(loop 
	   :for i from 1 upto 10 
	   :for (str _ xit-code) = (multiple-value-list (inferior-shell:run (format nil "/sbin/ip link show ~a.~a" pif *vlan-id*)  :on-error nil))
	   :until (eq xit-code 0)
	   :do (print i))
	(inferior-shell:run (format nil "/sbin/ip address add ~a/~a brd + dev ~a.~a" (numex:->dotted ip) cidr-block pif *vlan-id*) :on-error nil)
	*vlan-id*
	)
    (t (c)
      (format t "We caught a condition.~a~&" c)
      (values nil c))
    )
  )


(defun up-vlan (id)
  (handler-case
      (inferior-shell:run/s (format nil "/sbin/ip link set dev wlan.~a up" *vland-id*))
    (t (c)
      (format t "We caught a condition.~&")
      (values nil c))
    )
  )
  
(defun del-vlan (id)
  (handler-case
      (progn
	(inferior-shell:run/s (format nil "/sbin/ip set dev wlan0.~a down" id))
	(inferior-shell:run/s (format nil "/sbin/ip link delete wlan0.~a" id))
	)
    (t (c)
      (format t "We caught a condition.~&")
      (values nil c))
    )
  )

(defun del-addr (pif ip cidr-block)
  (handler-case
      (progn
	(inferior-shell:run/s (format nil "/sbin/ip address del ~a/~a brd + dev ~a" (numex:->dotted ip) cidr-block) pif)
	)
    (t (c)
      (format t "We caught a condition.~&")
      (values nil c))
    )
  )

(defun disable-xtalk (neta netb cidrb)
  (handler-case
      (progn
	(inferior-shell:run (format nil "/sbin/iptables -I FORWARD -s ~a/~a -d ~a/~a -j DROP"  neta cidrb netb cidrb))
	(inferior-shell:run (format nil "/sbin/iptables -I FORWARD -d ~a/~a -s ~a/~a -j DROP"  neta cidrb netb cidrb))
	)
    (t (c)
      (format t "We caught a condition.~a~&" c)
      (values nil c))
    )
  )
	

iptables -I FORWARD -s 192.168.1.0/24 -d 192.168.2.0/24 -j DROP

(defun add-addr (pif ip cidr-block)
  (handler-case
      (progn
	(inferior-shell:run (format nil "/sbin/ip address add ~a/~a brd + dev ~a" (numex:->dotted ip) cidr-block pif))
	)
    (t (c)
      (format t "We caught a condition.~a~&" c)
      (values nil c))
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
       "state" state
       "group" _
       "qlen" _
       type mac
       "brd" _
       "inet" ip/cidr
       rest)
      (make-instance 'ip-addr
		     :name if
		     :state state
		     :ltype type
		     :mac mac
		     :addr ip/cidr)))
   (ip-addr)))

(defun ip-link-objs ()
  (let ((links (ip-link)))
    (loop :for link in links :collect
       (let (
	     (iq  (serapeum:queue 'link :name (car link)))
	     ) ;;instance-queue	 
	 (loop :for f :on link :do
	  (trivia:match
	      f
	    ((list* "mtu" mtu _)  (serapeum:qappend iq `(:mtu ,(parse-integer mtu))))
	    ((list* "qdisc" qd _) (serapeum:qappend iq `(:qdisk ,qd)))
	    ((list* "state" state _) (serapeum:qappend iq `(:state ,state)))
	    ((list* "mode" mode _) (serapeum:qappend iq `(:mode ,mode)))
	    ((list* "group" group _) (serapeum:qappend iq `(:group ,group)))
	    ((list type mac "brd" brd) (serapeum:qappend iq `(:ltype ,type :broadcast ,brd :mac ,mac))))
	    )
	 (apply #'make-instance (serapeum:qlist iq))
	 ))
    )
  )
	    
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
