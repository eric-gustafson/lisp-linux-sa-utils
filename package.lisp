;;;; package.lisp

(defpackage #:lsa
  (:shadowing-import-from #:trivia @)
  (:use #:cl #:serapeum #:trivia #:trivia.ppcre #:cl-interpol)
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
   
   :link
   :name :mtu :qdisk :state :mode :group :mac :ltype :broadcast
   :addr

   :disable-xtalk
   :add-vlan
   :del-vlan
   :up-vlan
   :add-addr
   :del-addr

   ;; link object
   :name
   :path
   :mac
   :addr-len
   :hwtype
   
   :/sys->link-obj

   :git-update-all
   
   )
  )
