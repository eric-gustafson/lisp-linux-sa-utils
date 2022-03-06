;;;; package.lisp

(defpackage #:lsa
  (:shadowing-import-from #:trivia @)
  
  (:use #:cl #:cl-interpol)

  (:nicknames :wifi)   ;; should move to it's own package

  (:local-nicknames 
   (:al  :alexandria)
   (:a :alexandria)
   (:pm :trivia)
   ;;(:pm.extra :trivia.extra)
   (:pm.ppcre :trivia.ppcre)
   (:re :ppcre)
   (:pm.re :optima.ppcre)
   (:s :serapeum)
   (:r :rutils)
   )  
  
  (:export

   :alog-thunk
   
   :split-into-lines
   :chomp-and-count
   :iw-list-level
   
   :iw-dev-raw
   :iw-dev-simple
   :iw-list-tree
   :ip-link
   :iwconfig-interface-list
   :ip-addr
   :ifname->wireless-dev-num
   
   :hostapd ;; generates hostapd config file
   :hostapd-file
   :setup-hostapd

   :wifi-info
   :netdev-type
   :seq->tree
   
   :raspberry-pi?
   :ensure-monitor!!
   
   :disable-xtalk
   :add-vlan
   :del-vlan
   :up-vlan
   :add-addr
   :del-addr

   ;; link object
   :path

   
   :/sys->link-obj

   :git-update-all
   
   )
  )
