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
