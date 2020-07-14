(in-package #:lsatest)

(defparameter tseq (list 1 2 3 3 2 3 3 1 2 3 3 4 4 2 3 3 1 2))

(defparameter tseq-result
  `(1 (2 (3 3)
	  2 (3 3))
     1 (2 (3 3 (4 4))
	  2 (3 3))
     1 (2))
  )

(defparameter tseq-iw-dev
  '("phy#0"
    "	Unnamed/non-netdev interface"
    "		wdev 0x2"
    "		addr 48:45:20:ef:5b:e6"
    "		type P2P-device"
    "		txpower 0.00 dBm"
    "	Interface wlo1"
    "		ifindex 2"
    "		wdev 0x1"
    "		addr 48:45:20:ef:5b:e6"
    "		ssid g2"
    "		type managed"
    "		channel 40 (5200 MHz), width: 20 MHz, center1: 5200 MHz"
    "		txpower 22.00 dBm"
    "		multicast TXQ:"
    "			qsz-byt	qsz-pkt	flows	drops	marks	overlmt	hashcol	tx-bytes	tx-packets"
    "			0	0	0	0	0	0	0	0		0"
    ))

(fiasco:deftest seq2tree-vector-of-1 ()
  (equal (seq->tree #(1) :level-key #'values)
	 '(1)))

(fiasco:deftest seq2tree ()
  (let ((tree-from-seq (seq->tree tseq :level-key #'values)))
    (fiasco:is
     (equalp tree-from-seq tseq-result))
    )
  )


(fiasco:deftest strings()
  (seq->tree tseq-iw-dev :level-key #'lsa:iw-list-level :level-init 0)
  )

;; Todo:  Add and delete a test user for these tests, as part of the tests
;;

(fiasco:deftest uid ()
  (multiple-value-bind (ouid ogid)
      (lsa:get-uginfo)
    (cond
      ((eq ouid 0)
       (lsa:as-user* "egustafs"
		     #'(lambda()
			 (format t "~a~%" (multiple-value-list (lsa:get-uginfo))))
		     )
       )
      (t
       (fiasco:skip)))))

(fiasco:deftest envtest ()
  (multiple-value-bind (ouid ogid)
      (lsa:get-uginfo)
    (cond
      ((eq ouid 0)  
       (lsa:as-user
	   "egustafs"
	 (fiasco:is (equalp (uiop:getenv "USER") "egustafs")))
       (fiasco:is (equalp (uiop:getenv "USER") "root")))
      (t
       (fiasco:skip)))
    )
  )


(fiasco:deftest asusr ()
  ;; When run as-root, create a file as 
  (multiple-value-bind (ouid ogid)
      (lsa:get-uginfo)
    (cond
      ((eq ouid 0)
       (multiple-value-bind (file euid egid)
	   (lsa:as-user "egustafs"
	     (uiop/stream:with-temporary-file (:stream out
					       :pathname where
					       :keep t
					       )
	       (format out "this is a test~%")
	       (values where (iolib/syscalls:geteuid) (iolib/syscalls:getegid))))
	 (declare (ignorable file euid egid))
	 ;;(format t "temp file:~a~%" file)
	 (fiasco:is (probe-file file))
	 (let ((statObj (iolib/syscalls:stat (format nil "~a" (probe-file file)))))
	   (fiasco:is euid (iolib/syscalls:stat-uid statObj)))
	 ;; Make sure this process is back to normal
	 (fiasco:is (eq (iolib/syscalls:getuid) 0))
	 )
       )
      (t
       (fiasco:skip))
      )
    )
  )
