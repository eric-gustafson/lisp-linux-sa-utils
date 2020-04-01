
(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defun git-update-all ()
  "Does a git pull with --recurse-submodules and update --remote.  Brings all submodules up to date"
  #+nil(eazy-process:exec `("git" "pull"))
  (append
   (multiple-value-list (eazy-process:exec `("git" "pull" "--recurse-submodules")))
   (multiple-value-list (eazy-process:exec `("git" "submodule" "update" "--remote")))
   )
  )


