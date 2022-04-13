(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defun ip-routes ()
  "Calls the linux command ip route show"
  (multiple-value-bind (output-str error-str exit-status)
      (uiop-shell:run/s "ip route show")
    (declare (ignorable error-str exit-status))
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
      (uiop-shell:run/s "ip route delete ~a" default-route)
      )
    )
  )

(defun route-add-default-gw!! (cidr-net gateway-ip)
  (uiop-shell:run/s "ip route add ~a via ~a" cidr-net gateway-ip)
  )

(export '(ip-routes ip-default-route delete-default-route!! route-add-default-gw!!))

(defun clear-wifi-routes ()
  (loop :for route :in (ip-routes) :do
    (unless (search "eth0" route)
      (uiop:run-program (format nil "ip route del ~a" route))
      )
	)
  )

(export 'clear-wifi-routes)
