(defvar *handlers* ()
  "List of condition handlers.")
(defvar *resumes* ()
  "List of resumption points.")

(defun sr--tag-compatible (sig-tag han-tag)
  "Returns true if a signal with tag SIG-TAG can be caught by a handler with tag HAN-TAG."
  ;; todo: typecase rather than symbol equality
  (eq han-tag sig-tag))

(defun signal-resumable (tag data)
  (cl-mapl (lambda (tail)
	     (let ((han (car tail)))
	       (when (sr--tag-compatible tag (car han))
		 ;; ensure handlers are invisible to themselves when run
		 (dlet ((*handlers* (cdr tail)))
		   (apply (cdr han) tag data)))))
	   *handlers*)
  ;; if no handlers are found, then actually signal the condition properly
  (signal tag data))

(defun resume-from (res-name &rest data)
  (when-let ((res (assoc res-name *resumes*)))
    (apply (cdr res) data)))

(defmacro handler-bind (handlers &rest body)
  "Run BODY with the given HANDLERS in scope."
  `(sr--dprepend ((list ,@(mapcar (pcase-lambda (`(,tag ,fn))
			 `(cons ',tag ,fn))
		       handlers))
	      *handlers*)
     ,@body))

(defmacro restart-bind (restarts &rest body)
  "Run BODY with the given RESTARTS in scope."
  `(sr--dprepend ((list ,@(mapcar (pcase-lambda (`(,tag ,fn))
			 `(cons ',tag ,fn))
		       restarts))
	      *resumes*)
     ,@body))

(cl-defmacro sr--dprepend ((list place) &rest body)
  "Prepend LIST to the dynamic PLACE, but only within the dynamic extend of BODY. It is reset on exit."
  (declare (indent 1))
  `(dlet ((,place (append ,list ,place)))
     ,@body))

(cl-defmacro sr--dpush ((val place) &rest body)
  "Push VAL to the dynamic PLACE, but only within the dynamic extend of BODY. It is reset on exit."
  (declare (indent 1))
  `(dlet ((,place (cons ,val ,place)))
     ,@body))

(ert-deftest basic-restart ()
  "test a basci case of signal handling and restarting."
  (should
   (eq 42069
       (cl-labels ((throws-resumes (val handler)
		     (cl-block throws-resumes
		       (restart-bind ((try-again #'(lambda (num)
						   (cl-return-from throws-resumes
						     (funcall handler num)))))
				     (signal-resumable 'error `(,val))))))
	 (handler-bind
	     ((error (lambda (_sig data)
		       (resume-from 'try-again (+ data 42000)))))
	   (throws-resumes 68 (lambda (v) (+ 1 v))))))))
