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
   (thread :type thread
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
    (handler-bind ((t (lambda (exn)
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
  (:documentation "Wait for a PROMISE to resolve to a value. If an error occurred while trying to fulfill
the PROMISE, AWAIT will raise that error with the same restarts that were available in the PROMISE's thread.
Invoking one of these restarts will send a message back to the PROMISE thread telling it to invoke the
corresponding restart within the thread. If the stack is unwound without selecting a restart, then
the thread will be aborted."))

(defun await-internal (p)
  (let ((message (or (promise-resolution p)
		     (setf (promise-resolution p)
			   (get-message (promise-outbox p))))))
    (ecase (car message)
      (:values
       (apply #'values (cdr message)))
      (:error
       (let ((err (getf message :error))
	     (restarts (getf message :restarts)))
	 (cond ((promise-error p)
		(error (promise-error p)))
	       (t
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
			     (format t "Restart not invoked; Sending ABORT message~%")
			     (setf (promise-error ,p) ,err)
			     (send-message (promise-inbox ,p) '(abort)))))))))))))

(defmethod await ((p promise))
  (with-lock-held ((slot-value p 'mutex))
    (await-internal p)))

(defmacro lambda-async (lambda-list &body body)
  `(lambda ,lambda-list
     (make-instance 'promise :thunk (lambda ()
				      ,@body))))

(defmacro defun-async (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (make-instance 'promise :thunk (lambda () ,@body))))
