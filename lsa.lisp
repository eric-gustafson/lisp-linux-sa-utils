;;;; lisp-linux-sa-utils.lisp

(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)


(defvar alog-thunk nil)

(defun alog (str)
  (cond
    (alog-thunk
     (funcall alog-thunk str))
    (print str)))

(defclass link ()
  (
   (name :accessor name :initarg :name :initform "")
   (path :accessor path :initarg :path :initform nil)
   (mtu  :accessor mtu :initarg :mtu :initform "")
   (qdisk :accessor qdisk :initarg :qdisk :initform "")
   (state :accessor state :initarg :state :initform  "")
   (mode :accessor mode :initarg :mode :initform "")
   (group :accessor group :initarg :group :initform "")
   (mac :accessor mac :initarg :mac :initform "")
   (addr-len :accessor addr-len :initarg :addr-len :initform nil)
   (ltype :accessor ltype :initarg :ltype :initform "")
   (hwtype :accessor hwtype
	   :initarg :hwtype
	   :initform nil
	   :documentation "the hardware type which is found in the
	   /proc file system and is given as part of the dhcp/bootp hw
	   type ")
   (broadcast :accessor broadcast :initarg :broadcast :initform "")
   ;; (:accessor :initarg :initform)
   )
  )

(export '(link name mtu qdisk state mode group mac addr-len ltype hwtype broadcast))

(defun machine-info-model ()
  (loop :for e :in (uiop:read-file-lines #P"/proc/cpuinfo")
     :if (ppcre:scan "Model" e) :do
       (return e))
  )

(defun machine-info-string-to-type (str)
  "This is just me exploring what might be useful"
  (let ((sig (ppcre:split "\\s+" str)))
    (trivia:match
	sig
      ((list* "Raspberry" "Pi" rest)
       (values :raspberry-pi sig))
      ))
  )

(defun raspberry-pi? ()
  "returns a list of additional information about the raspberry-pi if true, nil otherwise"
  (let ((sig (ppcre:split "\\s+" (machine-info))))
    (trivia:match
	sig
      ((list* "Raspberry" "Pi" rst) rst)
      )))

(defun machine-info()
  (pm:match
      (machine-info-model)
    ((and (type string)
	  (pm.ppcre:ppcre "^Model[^:]+:\\s+(.*)" x))
     x)
    )
  )

#+nil(defun /sys/devices/platform*-dev-and-bus (path)
  "Given the parameter:

 /sys/devices/platform/scb/fd500000.pcie/pci0000:00/0000:00:00.0/0000:01:00.0/usb1/1-1/1-1.4/1-1.4:1.0/net/wlan1

this function returns two values: (usb-bus-number usb-device-number)
"
  )

#+nil(defun /dev/bus/usb-reset (hub-num dev-num)
  (let ((dev-path (format nil "/dev/bus/usb/~3,'0x/~3,'0x" hub-num dev-num)))
    (log4cl:log-error "resetting ~a" dev-path)
    (let ((a (osicat-posix:open dev-path 2)))
      (unwind-protect
	   ;;#define USBDEVFS_RESET             _IO('U', 20)   //usbdevice_fs.h
	   (osicat-posix:ioctl a 20)
	(osicat-posix:close a)
	))
    dev-path
    )
  )

;; TODO: This is not good for recovery since I bet the entry ofr /sys/class/net
;;  goes away when we loose the dongle.
(defun /sys->wireless ()
  "On Linux, under /sys/class/net all of the network devices are
  listed.  This function loops through those, filtering for wireless
  devices and then it returns a list that is the directory to that
  device, along with key information reading from the kernels devices info from the file system.
"
  (labels ((/sys->ieee80211-info (path)
	     (loop
	       :for wifi-info :in  `("addresses"
				     "address_mask"
				     "index"
				     "macaddress"
				     "name")
	       :collect (r:mkeyw  wifi-info) 
	       :collect
	       (uiop:safe-read-file-line  (merge-pathnames
					   wifi-info
					   path))))
	   )
    (loop :for dev :in  (uiop:subdirectories #P"/sys/class/net/")
	  :for it = (probe-file (merge-pathnames "wireless" dev))
	  :for phys = (probe-file (merge-pathnames "phy80211" dev))
	  :when it
	    :collect  (list dev (pathname-directory it)
			    (/sys->ieee80211-info phys)
			    ))
    )
  )

(export '/sys->wireless)

(defun search-dirRs-for-file (dir-path file-marker &key (stop-after-found))
  (s:with-collector (g)
    (labels ((search-dir (dir)
	       (r:when-it (probe-file (merge-pathnames file-marker dir))
		 (g r:it)
		 (if stop-after-found (return-from search-dir))
		 )
	       (when (equalp (truename dir) dir)
		 (loop
		   :for sd :in (uiop:subdirectories dir)
		   :do
		      (search-dir sd)
		   ))))
      (search-dir dir-path)
      )
    )
  )

(defun /sys/devicesR/*authorized ()
  (search-dirRs-for-file #P"/sys/devices/" "authorized" :stop-after-found t)
  )

(defun reset-path!! (path)
  (uiop/stream:with-output-file (out path :if-exists :append)    (princ  0 out))
  (uiop/stream:with-output-file (out path :if-exists :append)    (princ  1 out))
  )

(defun reset-all-usbs ()
  (loop :for up :in (/sys/devicesR/*authorized) :do
    (log4cl:log-debug "resetting usb device: ~a" up)
    (handler-case
	(progn
	  (log4cl:log-error "resetting usb device: ~a" up)
	  (reset-path!! up)
	  )
      (error (c)
	 (let ((bt (with-output-to-string (s)
		     (trivial-backtrace:print-backtrace-to-stream s))))
	   (log4cl:log-error "Unhandled seriours-condition of type ~A: ~A~%~A"
			     (type-of c) c bt)
	   )
	)
      )
    )
  )

(defun /sys-devices->wireless ()
  "Find all wireless devices underneath the devies subdirectory. "
  (s:with-collector (g)
    (labels ((search-dir (dir)
	       (cond
		 ((probe-file (merge-pathnames "wireless" dir))
		  (g dir))
		 ((equalp (truename dir) dir)
		  (loop
		    :for sd :in (uiop:subdirectories dir)
		    :do
		       (search-dir sd)
		    )))))
      (search-dir #P"/sys/devices/")
      )
    )
  )

(defun /sys->link-obj ()
  "Use the proc file system to return link objects for the system"
  (let (
	(stuff)
	(dir #P"/sys/class/net/*")
	)
    (loop :for de :in (uiop:directory* dir) :do
       (let ((name (car (reverse (pathname-directory de))))
	     (mac (car (uiop:read-file-lines (merge-pathnames de  "address"))))
	     (addrlen (parse-integer (car (uiop:read-file-lines (merge-pathnames de "addr_len")))))
	     (type (parse-integer (car (uiop:read-file-lines (merge-pathnames de "type")))))
	     )
	 (push (make-instance 'link
			      :mac mac
			      :name name
			      :path de
			      :hwtype type
			      :addr-len addrlen
			      )
	       stuff))
       )
    stuff)
  )

(defclass ip-addr (link)
  (
   (addr :accessor addr :initarg :addr :initform nil)
   (netmask :accessor netmask :initarg :netmask :initform nil)
   )
  )

(export `(addr netmask))

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
	  (name mtu qdisk state mode group mac ltype broadcast hwtype)
	obj
      (format stream "~a,~a,~a,state=~a,mode=~a,group=~a,~a,~a,~a,hwtype=~a"
	      name mtu qdisk state mode group
	      mac ltype broadcast hwtype
	      ))
    )
  )


(defmethod print-object ((obj ip-addr) stream)
  (print-unreadable-object
      (obj stream :type t)
    (with-slots
	  (name mtu qdisk state mode group mac ltype broadcast addr netmask)
	obj
      (format stream "~a,~a/~a,~a,~a,state=~a,mode=~a,group=~a,~a,~a,~a"
	      name addr netmask mtu qdisk state mode group
	      mac ltype broadcast
	      ))
    )
  )

(defun common-splitter (txt)
  "splits up sections of code from ip, such as 'ip addr' and 'ip
link'.  Returns a ((lo ...) (eth0 ...)) wher everything is a string."
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
	(uiop-shell:run/s "/sbin/ip link add link ~a name ~a.~a type vlan id ~a" pif pif *vlan-id*  *vlan-id*)
	(loop 
	   :for i from 1 upto 10 
	   :for (str _ xit-code) = (multiple-value-list (uiop-shell:run/s "/sbin/ip link show ~a.~a" pif *vlan-id*))
	   :until (eq xit-code 0)
	   :do (print i))
	(uiop-shell:run/s "/sbin/ip address add ~a/~a brd + dev ~a.~a" (numex:->dotted ip) cidr-block pif *vlan-id*)
	*vlan-id*
	)
    (t (c)
      (format t "We caught a condition.~a~&" c)
      (values nil c))
    )
  )

(defun up-vlan (id)
  (uiop-shell:run/s "/sbin/ip link set dev wlan.~a up" *vland-id*)
  )
  
(defun del-vlan (id)
  (uiop-shell:run/s "/sbin/ip link set dev wlan0.~a down" id)
  (uiop-shell:run/s "/sbin/ip link delete wlan0.~a" id)
  )

(defun del-addr (pif ip cidr-block)
  (uiop-shell:run/s "/sbin/ip address del ~a/~a brd + dev ~a" (numex:->dotted ip) cidr-block pif)
  )

(defun add-addr (pif ip cidr-block)
  (uiop-shell:run/s "/sbin/ip address add ~a/~a brd + dev ~a" (numex:->dotted ip) cidr-block pif)
  )


;; note-to-self:  Only drop the traffic for the default case when the target network is under the
;;  agents control, it's responsability.
(defun disable-xtalk (neta netb cidrb)
  #+nil(inferior-shell:run (format nil "/usr/sbin/iptables -I FORWARD -s ~a/~a -d ~a/~a -j DROP"  (numex:->dotted neta) cidrb (numex:->dotted netb) cidrb))
  #+nil(inferior-shell:run (format nil "/usr/sbin/iptables -I FORWARD -d ~a/~a -s ~a/~a -j DROP"  (numex:->dotted netb) cidrb (numex:->dotted neta) cidrb))
  ;; Drop all traffic to this device by default.
  (uiop-shell:run/s "/usr/sbin/iptables -I FORWARD -d ~a/~a -j DROP"  (numex:->dotted netb) cidrb (numex:->dotted neta) cidrb)
  )

(defun ip-link ()
  (multiple-value-bind (out err xit-status)
      (uiop-shell:run/s "/sbin/ip link")
    (declare (ignore err xit-status))
    (common-splitter out))
  )

(defun ip-addr ()
  "An uncached result.  Run's common-splitter over top of the command
'ip addr'"
  (multiple-value-bind (out err xit-status)
      (uiop-shell:run/s "/sbin/ip addr")
    (declare (ignore err xit-status))
    (common-splitter out))
  )

(defun ip-addr-from-text-list (text-lst)
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
      (trivia:match
	  (ppcre:split "/" ip/cidr)
	((list ip nmask)
	 (make-instance 'ip-addr
			:name if
			:state state
			:ltype type
			:mac mac
			:addr ip
			:netmask nmask))))
     ((list*
       if _
       "mtu" _
       "qdisc" _
       "state" state
       "group" _
       "qlen" _
       type mac
       "brd" _
       ;;"inet" ip/cidr
       rest)
      (make-instance 'ip-addr
		     :name if
		     :state state
		     :ltype type
		     :mac mac
		     :addr nil
		     :netmask nil))
      
     )
   text-lst))
(export 'ip-addr-from-text-list)


(defun ip-addr-objs ()
  (ip-addr-from-text-list (ip-addr))
  )

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

(export '(ip-addr-objs ip-link-objs))

	    
(defparameter *if-scanner-splitter* (ppcre:create-scanner "^\\d+:" :multi-line-mode t))
(defparameter *if-scanner* (ppcre:create-scanner "^\\d+:\\s+([^:]+):" :multi-line-mode t))
(defparameter *ip-addr-scanner* (ppcre:create-scanner ".*inet\\s+(\\d+\\.\\d+\\.\\d+\\.\\d+)/(\\d+)" :multi-line-mode t))
(defparameter *mac-addr-scanner* (ppcre:create-scanner ".*link/\\w+\\s+(\\S+)" :multi-line-mode t))


(export '*ip-addr-scanner*)

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
    (multiple-value-bind (out err xit-code)
	(uiop-shell:run/s "iwconfig")
      (declare (ignore err xit-code))
      (loop :for line :in (ppcre:split "(\\n|\\r)" out)
	    :do
	       (trivia:multiple-value-match
		   (ppcre:scan-to-strings "^(\\w+)\\s.*" line)
		 ((_ #(first))
		  (push first results)))))
    results))


(defmethod add-route ( (target ip-addr) (via ip-addr) )
  (uiop-shell:run/s "ip route add ~a via ~a dev eth0" target ip-addr)
  )


