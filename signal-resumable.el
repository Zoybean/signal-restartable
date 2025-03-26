(defvar *handlers* ()
  "List of condition handlers.")
(defvar *resumes* ()
  "List of resumption points.")

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

(defun sr--tag-compatible (sig-tag han-tag)
  "Returns true if a signal with tag SIG-TAG can be caught by a handler with tag HAN-TAG."
  ;; todo: typecase rather than symbol equality
  (eq han-tag sig-tag))

(defun signal-resumable (tag data)
  "Signal a resumable condition TAG with additional data list DATA.

If there are no applicable signal handlers, calls `signal' to signal the condition normally."
  (cl-mapl (lambda (tail)
	     (let ((han (car tail)))
	       (when (sr--tag-compatible tag (car han))
		 ;; ensure handlers are invisible to themselves when run
		 (dlet ((*handlers* (cdr tail)))
		   (apply (cdr han) tag data)))))
	   *handlers*)
  ;; if no handlers are found, then actually signal the condition properly
  (signal tag data))

(defun invoke-restart (res-name &rest data)
  "If a restart RES-NAME is dynamically active, call it."
  (if-let ((res (assoc res-name *resumes*)))
      (apply (cdr res) data)
    (signal 'control-error (list "no such restart" res-name))))

(defmacro handler-bind (handlers &rest body)
  "Run BODY with the given HANDLERS in scope."
  (declare (indent 1))
  `(sr--dprepend ((list ,@(mapcar (pcase-lambda (`(,tag ,fn))
			 `(cons ',tag ,fn))
		       handlers))
	      *handlers*)
     ,@body))

(defmacro restart-bind (restarts &rest body)
  "Run BODY with the given RESTARTS in scope."
  (declare (indent 1))
  `(sr--dprepend ((list ,@(mapcar (pcase-lambda (`(,tag ,fn))
			 `(cons ',tag ,fn))
		       restarts))
	      *resumes*)
     ,@body))

(cl-defmacro restart-case (body &rest restarts)
  ;; TODO unwind the stack before running restarts
  (let ((block (gensym)))
    `(cl-block ,block
       (restart-bind
	   ,(mapcar (pcase-lambda (`(,name ,args . ,body))
		      `(,name (lambda ,args (cl-return-from ,block (progn ,@body)))))
		    restarts)
       ,body))))

(cl-defmacro handler-case (body &rest handlers)
  ;; TODO unwind the stack before running handlers
  (let ((block (gensym)))
    `(cl-block ,block
       (handler-bind
	   ,(mapcar (pcase-lambda (`(,name ,args . ,body))
		      `(,name (lambda ,args (cl-return-from ,block (progn ,@body)))))
		    handlers)
       ,body))))


(ert-deftest basic-restart ()
  "Test a basic case of signal handling and restarting."
  (should
   (eq 42069
       (cl-labels ((throws-resumes (val)
		     (cl-block throws-resumes
		       (restart-bind ((try-again #'(lambda (num)
						   (cl-return-from throws-resumes
						     (+ 1 num)))))
				     (signal-resumable 'error `(,val))))))
	 (handler-bind
	     ((error (lambda (_sig data)
		       (invoke-restart 'try-again (+ data 42000)))))
	   (throws-resumes 68))))))

(ert-deftest basic-restart-2 ()
  "Based on https://lisp-docs.github.io/cl-language-reference/chap-9/j-c-dictionary/restart-case_macro."
  (should
   (eq 7
       (restart-case
	(handler-bind ((error #'(lambda (c _cond)
				  (invoke-restart 'use-value 7))))
	  (signal-resumable 'error '("Foo.")))
	(use-value (&optional v) v)))))
