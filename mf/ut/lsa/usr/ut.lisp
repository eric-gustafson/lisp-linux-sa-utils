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
