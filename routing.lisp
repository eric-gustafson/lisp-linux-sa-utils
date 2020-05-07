(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defun ip-routes ()
  "Calls the linux command ip route show"
  (multiple-value-bind (output-str error-str exit-status)
      (eazy-process:exec '("ip" "route" "show"))
    (loop :for line :in (serapeum:lines output-str) :collect line)
    ))

(defun ip-default-route ()
  "Calls the ip route show command, and returns the default route"
  (loop :for route :in (ip-routes)
	:if (search "default" route) :do
	(return-from ip-default-route route))
  )

(defun delete-default-route!! ()
  (let ((default-route (serapeum:tokens (ip-default-route))))
    (when default-route
      (eazy-process:exec (nconc
			  '("ip" "route" "delete" )
			  default-route))
      )
    )
  )
    


(defun route-add-default-gw!! (cidr-net gateway-ip)
  (multiple-value-bind (output-str error-str xit-status)
      (eazy-process:exec `("ip" "route" "add" ,cidr-net "via" ,gateway-ip))
    )
  )

(export '(ip-routes ip-default-route delete-default-route!! route-add-default-gw!!))


