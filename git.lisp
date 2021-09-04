
(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defun git-update-all ()
  "Does a git pull with --recurse-submodules and update --remote.  Brings all submodules up to date"
  #+nil(eazy-process:exec `("git" "pull"))g
  ;(uiop-shell:lshm "git pull --recurse-submodules && git submodule update --remote")
  (uiop-shell:&& `("git pull --recurse-submodules")
		 `("git submodule update --remote"))
  )


