(in-package :cl-async-await)

(defclass promise ()
  ((resolution :type t
	       :initarg :resolved
	       :initform nil
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

(defclass immediate-promise (promise) ())

(defgeneric force (promise))

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

(defgeneric then (promise thunk))

(defmethod then ((promise promise) thunk)
  (when (promise-resolved-p promise)
    (apply thunk (promise-resolution promise)))
  (setf (promise-continuation promise) thunk)
  promise)

(defmethod then ((not-promise t) thunk)
  (funcall thunk not-promise))

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

(defmethod catch-exception ((not-promise t) condition-type thunk)
  nil)
