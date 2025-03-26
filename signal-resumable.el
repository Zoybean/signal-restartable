(defvar *handlers* ()
  "List of condition handlers.")
(defvar *resumes* ()
  "List of resumption points.")

(defun tag-compatible (sig-tag han-tag)
  ;; todo: typecase rather than symbol equality
  (eq han-tag sig-tag))

(defun signal-resumable (tag data)
  (message "current handlers: %s" *handlers*)
  (cl-mapl (lambda (tail)
	     (let ((han (car tail)))
	       (when (tag-compatible tag (car han))
		 (message "found compatible handler %s, running %s" han (cdr han))
		 ;; ensure handlers are invisible to themselves when run
		 (dlet ((*handlers* (cdr tail)))
		   (apply (cdr han) tag data)))))
	   *handlers*)
  ;; if no handlers are found, then actually signal the condition properly
  (signal tag data))

(defun resume-from (res-name &rest data)
  (message "current resumes: %s" *resumes*)
  (when-let ((res (assoc res-name *resumes*)))
    (message "found compatible restart %s, running %s" res (cdr res))
    (apply (cdr res) data)))

(defmacro handler-bind (handlers &rest body)
  `(dprepend ((list ,@(mapcar (pcase-lambda (`(,tag ,fn))
			 `(cons ',tag ,fn))
		       handlers))
	      *handlers*)
     ,@body))

(defmacro restart-bind (restarts &rest body)
  `(dprepend ((list ,@(mapcar (pcase-lambda (`(,tag ,fn))
			 `(cons ',tag ,fn))
		       restarts))
	      *resumes*)
     ,@body))

(cl-defmacro dprepend ((list place) &rest body)
  "Prepend LIST to the dynamic PLACE, but only within the dynamic extend of BODY. It is reset on exit."
  (declare (indent 1))
  `(dlet ((,place (append ,list ,place)))
     ,@body))

(cl-defmacro dpush ((val place) &rest body)
  "Push VAL to the dynamic PLACE, but only within the dynamic extend of BODY. It is reset on exit."
  (declare (indent 1))
  `(dlet ((,place (cons ,val ,place)))
     ,@body))

(cl-defun error-thing ()
  (restart-bind ((try-again (lambda (num)
			      (cl-return-from error-thing
				(+ 1 num)))))
    (signal-resumable 'error '(68))))

(handler-bind
    ((error (lambda (_sig data)
	      (resume-from 'try-again (+ data 42000)))))
  (error-thing))

