(in-package #:lsa-test)

(defparameter *fstr*
  (dict :a "5250 MHz [50] (disabled)"
	:b "5240 MHz [48] (23.0 dBm)" )
  )

(fiasco:deftest parse-freq-string ()
  (fiasco:is (eq 50 (lsa:extract-channel (gethash :a *fstr* ))))
  )

(export 'parse-freq-string)

(defparameter *fl-fg*
  `("5170 MHz [34] (disabled)"
    "5180 MHz [36] (20.0 dBm)"
    "5190 MHz [38] (disabled)" "5200 MHz [40] (20.0 dBm)"
   "5210 MHz [42] (disabled)" "5220 MHz [44] (20.0 dBm)"
   "5230 MHz [46] (disabled)" "5240 MHz [48] (20.0 dBm)"
   "5260 MHz [52] (20.0 dBm) (no IR, radar detection)"
   "5280 MHz [56] (20.0 dBm) (no IR, radar detection)"
   "5300 MHz [60] (20.0 dBm) (no IR, radar detection)"
   "5320 MHz [64] (20.0 dBm) (no IR, radar detection)"
   "5500 MHz [100] (20.0 dBm) (no IR, radar detection)"
   "5520 MHz [104] (20.0 dBm) (no IR, radar detection)"
   "5540 MHz [108] (20.0 dBm) (no IR, radar detection)"
   "5560 MHz [112] (20.0 dBm) (no IR, radar detection)"
   "5580 MHz [116] (20.0 dBm) (no IR, radar detection)"
   "5600 MHz [120] (20.0 dBm) (no IR, radar detection)"
   "5620 MHz [124] (20.0 dBm) (no IR, radar detection)"
   "5640 MHz [128] (20.0 dBm) (no IR, radar detection)"
   "5660 MHz [132] (20.0 dBm) (no IR, radar detection)"
   "5680 MHz [136] (20.0 dBm) (no IR, radar detection)"
   "5700 MHz [140] (20.0 dBm) (no IR, radar detection)"
   "5720 MHz [144] (20.0 dBm) (no IR, radar detection)"
   "5745 MHz [149] (20.0 dBm)" "5765 MHz [153] (20.0 dBm)"
   "5785 MHz [157] (20.0 dBm)" "5805 MHz [161] (20.0 dBm)"
   "5825 MHz [165] (20.0 dBm)"))

(defparameter *fl-2g*
  `("2412 MHz [1] (20.0 dBm)" "2417 MHz [2] (20.0 dBm)"
			      "2422 MHz [3] (20.0 dBm)" "2427 MHz [4] (20.0 dBm)"
			      "2432 MHz [5] (20.0 dBm)" "2437 MHz [6] (20.0 dBm)"
			      "2442 MHz [7] (20.0 dBm)" "2447 MHz [8] (20.0 dBm)"
			      "2452 MHz [9] (20.0 dBm)" "2457 MHz [10] (20.0 dBm)"
			      "2462 MHz [11] (20.0 dBm)" "2467 MHz [12] (disabled)"
			      "2472 MHz [13] (disabled)" "2484 MHz [14] (disabled)")
  )

(fiasco:deftest extract-freq ()
  (fiasco:is (eq
	      2412
	      (lsa:extract-MHz (car *fl-2g*))
	      )))

(fiasco:deftest channel-freq-table ()
  (fiasco:is
      (every #'(lambda(el)
		 (and (>= el 2000 )
		      (<= el 3000 )
		      ))
	     (mapcar #'lsa:extract-MHz *fl-2g*))
      )
  )


(fiasco:deftest dBm-values ()
  "parse a disabled and a valid entry from the 'frequencies' entries
of the iw list command"
  (and (fiasco:is (eq nil (lsa:extract-dBm (car *fl-fg*))))
       (fiasco:is (eq 20.0 (lsa:extract-dBm (cadr *fl-fg*))))
       )
  )

(export 'dBm-values)

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
