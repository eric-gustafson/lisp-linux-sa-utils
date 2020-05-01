(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defun ip-routes ()
  (multiple-value-bind (output-str error-str exit-status)
      (eazy-process:exec '("ip" "route" "show"))
    (loop :for line :in (ppcre:split "\\r|\\n" output-str) :collect line)
    ))
(export 'ip-routes)

(defun route-add (cidr-net gateway-ip)
  (multiple-value-bind (output-str error-str xit-status)
      (eazy-process:exec `("ip" "route" "add" ,cidr-net "via" ,gateway-ip))
    )
  )

(export 'route-add)

