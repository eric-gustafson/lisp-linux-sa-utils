(in-package #:lsa)

;; I've implemented a simple tree class that allows us to add/update a tree using a
;; ptr, and a simple interface

;; This should be moved to a more general package, and expanded if this ends up
;; being of use.

(defclass tree ()
  (
   (root :accessor root :initform nil :initarg :root)
   (stack :accessor stack :initform nil :initarg :stack)
   (ptr :accessor ptr :initform nil :initarg :ptr)
   )
  )

(defmethod print-object ((obj tree)  out)
  ;; 12/17/17 -- removing this.  no more frame-slot
  (print-unreadable-object (obj out :type t)
    (with-slots (root stack ptr) obj
      (format out "[root: ~a, stack: ~a, ptr:~a]"
	      root stack ptr)
      ))
  )

(defmethod add! ((obj tree) &rest rest)
  (unless (every #'atom rest)
    (error "You may only add atoms"))
  (with-slots (root stack ptr) obj
    (setf ptr (nconc ptr (copy-list rest)))
    (unless root (when ptr (setf root ptr)))    
    )
  )

(defmethod del! ((obj tree) &rest objs)
  (unless (every #'atom objs)
    (error "you may only delete atoms"))
  (with-slots (root stack ptr) obj
    (setf ptr (loop :for e :in ptr :unless (member e objs :test #'equal) :collect e))
    )
  )

(defun check-for-valid-path (path)
  (unless (every #'atom path)
    (error "Invalid path ~a" path)))


(defun cd! (obj path &key if-not-exist)
  (cond
    ((listp path)
     (check-for-valid-path path)
     (with-slots (root stack ptr) obj
       (setf ptr root)
       (loop :for e :in path :do
	    (cd! obj e :if-not-exist if-not-exist))
       ))
    ((stringp path)
     (with-slots (root stack ptr) obj
       (cond
	 ((equal "/" path)
	  (setf stack nil)
	  (setf ptr root)
	  )
	 ((equal ".." path)
	  (when stack
	    (setf ptr (pop stack))))
	 (t
	  (let (found)
	    (loop :for e :in ptr
	       :do
		 (when (and (consp e)
			    (equal (car e) path))
		   (push ptr stack)
		   ;; move the pointer to this new 'directory'
		   (setf ptr e)
		   (setf found t)
		   (loop-finish))
		 )
	    (unless found
	      (cond
		((eq :create if-not-exist)
		 (mkdir! obj path)
		 (cd! obj path)
		 )
		(t
		 (error "Could not find subtree ~a" path))
		))
	    )
	  )
	 )
       )
     )
    (t
     (error "Unexpected parameter: ~a~&" path))
    )
  )

(defmethod mkdir! ((obj tree) (dirname string))
  (with-slots (root stack ptr) obj
    (cond
      ((null root)
       (setf root `((,dirname)))
       (setf ptr root))
      (t
       (when (consp ptr)
	 (let ((nxt (cdr ptr)))
	   (setf (cdr ptr) (cons (list dirname) nxt)))
	 )
       )
      )
    )
  )


