(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defun ip-routes ()
  "Calls the linux command ip route show"
  (multiple-value-bind (output-str error-str exit-status)
      (run-fcmd "ip route show")
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
      (run-fcmd "ip route delete ~a" default-route)
      )
    )
  )

(defun route-add-default-gw!! (cidr-net gateway-ip)
  (run-fcmd "ip route add ~a via ~a" cidr-net gateway-ip)
  )

(defgeneric route-add-cidr-via (cidr-net via-ip)
  (:method ((cidr-net string) (via-ip string))
    (uiop:run-program (format nil "ip route add ~a via ~a" cidr-net via-ip) :output :string :error-output :string)
    )
  )

(export '(ip-routes ip-default-route delete-default-route!! route-add-default-gw!! route-add-cidr-via))

(defun clear-wifi-routes ()
  (loop :for route :in (ip-routes) :do
    (unless (search "eth0" route)
      (uiop:run-program (format nil "ip route del ~a" route))
      )
	)
  )

(export 'clear-wifi-routes)
