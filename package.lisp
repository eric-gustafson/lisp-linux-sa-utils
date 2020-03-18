;;;; package.lisp

(defpackage #:lsa
  (:shadowing-import-from #:trivia @)
  (:use #:cl #:serapeum #:trivia #:trivia.ppcre)
  (:export

   :iw-dev-raw
   :iw-dev-simple
   :ip-link
   :iwconfig-interface-list
   :ip-addr
   :hostapd

   :wifi-info
   
   :raspberry-pi?
   :ensure-monitor!!
   
   :link
   :name :mtu :qdisk :state :mode :group :mac :ltype :broadcast
   :ip-addr-objs
   :ip-link-objs
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
   
   )
  )
