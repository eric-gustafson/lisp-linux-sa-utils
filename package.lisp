;;;; package.lisp

(defpackage #:lsa
  (:use #:cl #:serapeum)
  (:export
   :ip-link
   :iwconfig-interface-list
   :ip-addr
   :hostapd
   :link
   :name :mtu :qdisk :state :mode :group :mac :ltype :broadcast
   :ip-addr-objs
   :ip-link-objs
   :addr

   :add-vlan
   :del-vlan
   :up-vlan
   :add-addr
   :del-addr
   
   )
  )
