
(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(sex:defun git-update-all ()
  "Does a git pull with --recurse-submodules and update --remote.  Brings all submodules up to date"
  #+nil(eazy-process:exec `("git" "pull"))
  ;;(uiop-shell:lshm "git pull --recurse-submodules && git submodule update --remote")
  (i:iter
    (i:for cmd :in (list "git pull --recurse-submodules" "git submodule update --remote"))
    (uiop:run-program cmd))
  )


