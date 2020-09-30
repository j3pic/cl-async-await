(in-package :cl-async-await)

(defclass promise ()
  ((resolution :type list
	       :initarg :resolution
	       :initform nil
	       :documentation "The final values that the promise generated. 
Only valid if the RESOLVEDP slot is non-nil."
	       :accessor promise-resolution)
   (mutex :type lock
	  :initform (make-lock "PROMISE-MUTEX"))
   (error :type (or condition null)
	  :initarg :error
	  :initform nil
	  :accessor promise-error)
   (thread :type (or thread null)
	   :initarg :thread
	   :initform nil
	   :accessor promise-thread)
   (outbox :type simple-actors/ipc::simple-process-mailbox
	   :initform (simple-actors/ipc:make-mailbox)
	   :reader promise-outbox)
   (inbox :type simple-actors/ipc::simple-process-mailbox
	  :initform (simple-actors/ipc:make-mailbox)
	  :reader promise-inbox)
   (thunk :type function
	  :initarg :thunk
	  :reader promise-thunk)))

(defmethod print-object ((promise promise) stream)
  (let ((resolution-string (acond ((promise-error promise)
				   (format nil " Promise broken due to error: ~a" it))
				  ((promise-resolution promise)
				   (format nil " ~a " it))
				  (t " Not awaited"))))
				  
    (format stream "#<~s~a>"
	    'promise resolution-string)))

(defun make-promise-handler (p)
  (lambda ()
    (handler-bind ((error (lambda (exn)
			    (send-message (promise-outbox p)
					  `(:error ,exn
					    :restarts ,(loop for r in (compute-restarts)
							  collect `(:name ,(restart-name r)
								    :report ,(format nil "~a" r)))))
			    (let ((restart-invocation (get-message (promise-inbox p))))
			      (apply #'invoke-restart restart-invocation)))))
      (restart-case
	  (let ((result
		 (multiple-value-list (funcall (promise-thunk p)))))
	    (send-message (promise-outbox p)
			  `(:values ,@result)))
	(abort ()
	  :report "Terminate the promise's thread."
	  (send-message (promise-outbox p) '(:values)))))))

(defmethod initialize-instance :after ((p promise) &key)
  (setf (promise-thread p)
	(make-thread (make-promise-handler p)
		     :name "PROMISE-THREAD")))
			 

(defgeneric await (promise)
  (:documentation "Wait for a PROMISE to resolve to one or more values. If the promise
succeeds, the values will be returned using CL:VALUES.

If an error occurs in the PROMISE thread, that error will be signalled in the thread
from which AWAIT is called, in a context where all the same restarts are defined
as are defined in the PROMISE thread. If INVOKE-RESTART is called with one of the
restarts defined in the PROMISE thread, that restart will be invoked in the PROMISE
thread, and AWAIT will return that restart's value form. 

If the stack frame for the call to AWAIT is unwound without invoking a restart,
the PROMISE thread will invoke its CL:ABORT restart.

Whether the PROMISE succeeds or fails, the result is memoized. Calling AWAIT a second time
on the same PROMISE will yield the same values.

If an error occurred and AWAIT is called a second time, the restarts will not be available,
since the PROMISE thread is expected to be dead as a result of invoking the ABORT restart."))

(defun raise-error-with-restarts (promise err restarts)
  (let ((p promise))
    (eval `(let ((restart-invoked nil))
	     (unwind-protect
		  (restart-case (error ,err)
		    ,@(loop for restart-description in restarts
			 collect `(,(getf restart-description :name)
				    (&rest restart-arguments)
				    :report ,(getf restart-description :report)
				    (setf restart-invoked t)
				    (send-message ,(promise-inbox p)
						  (cons ',(getf restart-description :name)
							restart-arguments))
				    (setf (promise-resolution ,p) nil)
				    (await-internal ,p))))
	       (unless restart-invoked
		 (setf (promise-error ,p) ,err)
		 (send-message (promise-inbox ,p) '(abort))))))))

(defun await-internal (p)
  (aif (promise-error p)
       (error it)
       (let ((message (or (promise-resolution p)
			  (setf (promise-resolution p)
				(get-message (promise-outbox p))))))
	 (ecase (car message)
	   (:values
	    (apply #'values (cdr message)))
	   (:error
	    (raise-error-with-restarts p (getf message :error)
				       (getf message :restarts)))))))

(defmethod await ((p promise))
  (with-lock-held ((slot-value p 'mutex))
    (await-internal p)))

(defmacro lambda-async (lambda-list &body body)
  "Creates a closure that creates a PROMISE when FUNCALLed. The BODY
will run in its own thread.

See also: AWAIT"
  `(lambda ,lambda-list
     (make-instance 'promise :thunk (lambda ()
				      ,@body))))

(defmacro defun-async (name lambda-list &body body)
  "Like LAMBDA-ASYNC but expands to a CL:DEFUN form instead of CL:LAMBDA."
  `(defun ,name ,lambda-list
     (make-instance 'promise :thunk (lambda () ,@body))))
