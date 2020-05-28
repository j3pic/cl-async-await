(in-package :cl-async-await)

(defclass promise ()
  ((resolution :type list
	       :initarg :resolution
	       :initform nil
	       :documentation "The final values that the promise generated. 
Only valid if the RESOLVEDP slot is non-nil."
	       :accessor promise-resolution)
   (resolvedp :type boolean
	      :initform nil
	      :accessor promise-resolved-p)
   (continuation :type function
		 :initarg :continuation
		 :initform #'values
		 :accessor promise-continuation)
   (error-continuations :type list
			:initform nil
			:accessor error-continuations)
   (error :type (or condition null)
		:initarg :error
		:initform nil
		:accessor promise-error)
   (thunk :type function
	  :initarg :thunk
	  :reader promise-thunk)))

(defclass immediate-promise (promise) ()
  (:documentation "A PROMISE that is FORCEd upon creation."))

(defclass parallel-promise (promise)
  ((mutex :type lock)))

(defgeneric force (promise)
  (:documentation "Invokes the thunk attached to the PROMISE. The promise will then be
either resolved or broken."))

(defmethod force ((promise promise))
  (unless (or (promise-resolved-p promise)
	      (promise-error promise))
    (handler-case
	(funcall (promise-thunk promise)
		 (lambda (&rest values)
		   (setf (promise-resolution promise)
			 values)
		   (setf (promise-resolved-p promise) t)))
      (t (exn)
	(loop for (type . thunk) in (error-continuations promise)
	   when (typep exn type)
	   return (funcall thunk exn))
	(setf (promise-error promise) exn)
	(setf (promise-resolution promise) nil)))
    (if (promise-resolved-p promise)
	(apply (promise-continuation promise)
	       (promise-resolution promise))))
  promise)

(defmethod force ((promise parallel-promise))
  (with-lock-held ((slot-value promise 'mutex))
    (call-next-method)))

(defmethod print-object ((promise promise) stream)
  (let ((resolution-string (cond ((promise-resolved-p promise)
				  (format nil " Resolved to: ~s" (promise-resolution promise)))
				 ((promise-error promise)
				  (format nil " Promise broken due to error: ~a" (promise-error promise)))
				 (t " Pending")))
	(continuation-string (aif (promise-continuation promise)
				  (format nil " Continuation: ~s" it)
				  "")))
				  
    (format stream "#<~s~a;~a; ~a error continuations>"
	    'promise resolution-string continuation-string
	    (length (error-continuations promise)))))

(defmethod initialize-instance :after ((p immediate-promise) &key)
  (force p))

(defmethod initialize-instance :after ((p parallel-promise) &key)
  (make-thread (lambda ()
		 (force p))
	       :name "PARALLEL-PROMISE thread"))  

(defgeneric then (promise thunk)
  (:documentation "Returns a new promise that will be forced when the first PROMISE's
continuation is called. The new promise resolves to the value of THUNK,
which will receive all the parameters of the continuation."))

(defmethod then ((promise promise) thunk)
  (let* ((continuation-params nil)
	 (new-promise (make-instance 'promise :thunk (lambda (resolve)
						       (funcall resolve (apply thunk continuation-params)))))
	 (continuation (lambda (&rest values)
			 (setf continuation-params values)
			 (force new-promise))))
    (setf (promise-continuation promise) continuation)
    (when (promise-resolved-p promise)
      (apply (promise-continuation promise)
	     (promise-resolution promise)))
    new-promise))

(defmethod then ((promise parallel-promise) thunk)
  (with-lock-held ((slot-value promise 'mutex))
    (call-next-method)))

(defmethod then ((not-promise t) thunk)
  (make-instance 'immediate-promise
		 :thunk (lambda (resolve)
			  (funcall resolve
				   (funcall thunk not-promise)))))

(defgeneric catch-exception (promise condition-type thunk))

(defmethod catch-exception ((promise promise) condition-type thunk)
  (when (and (promise-error promise)
	     (typep (promise-error promise) condition-type))
    (funcall thunk (promise-error promise)))
  (aif (assoc condition-type (error-continuations promise))
       (setf (cdr it) thunk)
       (push (cons condition-type thunk)
	     (error-continuations promise)))
  promise)

(defmethod catch-exception ((promise parallel-promise) condition-type thunk)
  (with-lock-held ((slot-value promise 'mutex))
    (call-next-method)))

(defmethod catch-exception ((not-promise t) condition-type thunk)
  nil)
