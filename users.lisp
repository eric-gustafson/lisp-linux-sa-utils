(in-package #:lsa)

(named-readtables:in-readtable :interpol-syntax)

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
    (multiple-value-bind (name x uid gid desc home shell)
	(iolib/syscalls:getpwnam user-name-as-str)
      (declare (ignorable name x uid gid desc home shell))
      (unwind-protect
	   (progn
	     (iolib/syscalls:setegid gid)	     
	     (iolib/syscalls:seteuid uid)
	     (funcall thunk))
	(iolib/syscalls:seteuid original-uid)	
	(iolib/syscalls:setegid original-gid)
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

