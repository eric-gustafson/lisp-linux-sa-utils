
(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defun update-all ()
  (eazy-process:exec `("git" "pull" "--recurse-submodules"))
  (eazy-process:exec `("git" "submodule" "update" "--remote"))
  )


