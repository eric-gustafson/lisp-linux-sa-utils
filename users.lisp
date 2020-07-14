(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

(defun env ()
  "Get a list of all of the environemnt variables, and their values"
  (let ((envptr (iolib/syscalls:os-environ)))
    (unless (cffi:null-pointer-p envptr)
      (loop :for i :from 0 :by 1
            :for string := (cffi:mem-aref envptr :string i)
            :while string :collect string))))
(export 'env)

(defun split-env-line (string)
  (let ((i (position #\= string)))
	  (cons
	   (subseq string 0 i)
	   (subseq string (1+ i)))))

(defun txt->env-alist (txt)
  (with-input-from-string (inport txt)
    (loop :for line = (read-line inport nil nil) :while line
	  :collect (split-env-line line))))

(export 'txt->env-alist)

(defun env-alist ()
  (loop :for string :in (env)
	:collect
	(split-env-line string)
	))
(export 'env-alist)			   

(defun alist->env (env-alst &key clearenv)
  "Replace the current env with alst.  If clearenv is true, then clear the environment before setting"
  (when clearenv (iolib/syscalls:clearenv))
  (loop :for (n . v) :in env-alst :do
    (iolib/syscalls:setenv n v t))
  )

(export 'alist->env)

(defun get-uginfo ()
  "Returns the UID and the GID as values"
  (values (iolib/syscalls:getuid)
	  (iolib/syscalls:getgid)
	  (iolib/syscalls:geteuid)
	  (iolib/syscalls:getegid)))

(export 'get-uginfo)

(defun as-user* (user-name-as-str thunk)
  "Calls the thunk as the user named in user-name-as-str.  This makes
UNIX system calls to get the current user id and group id, then to
change to the target user and group ids to funcall the thunk.  It then
unwind-protects back to the original uid and gid."
  (multiple-value-bind (original-uid original-gid)
      (get-uginfo)
    (let ((original-env (env-alist)))
      (multiple-value-bind (name x uid gid desc home shell)
	  (iolib/syscalls:getpwnam user-name-as-str)
	(declare (ignorable name x uid gid desc home shell))
	(unless (stringp name) (error "user ~a not found" user-name-as-str))
	(iolib/syscalls:setenv "USER" name t)
	(iolib/syscalls:setenv "HOME" home t)
	(iolib/syscalls:setenv "SHELL" shell t)
	(unwind-protect
	     (progn
	       (iolib/syscalls:setegid gid)	     
	       (iolib/syscalls:seteuid uid)
	       (funcall thunk))
	  (iolib/syscalls:seteuid original-uid)	
	  (iolib/syscalls:setegid original-gid)
	  (alist->env original-env :clearenv t)
	  )
	)
      )
    )
  )

(export 'as-user*)

(defmacro as-user (user-name-as-str &body body)
  "Run the code (body) as the user named in user-name-as-str.  This
makes UNIX system calls to get the current user id and group id, then
to change to the target user and group ids to execute the body, and
the unwind-protects back to the original uid and gid."
  `(as-user* ,user-name-as-str
	   #'(lambda()
	       ,@body))
  )

(export 'as-user)

