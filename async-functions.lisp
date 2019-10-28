(in-package :cl-async-await)

(defmacro with-declare-form (var function-body &body body)
  `(let ((,var (if (and (consp (car ,function-body))
			(eq (caar ,function-body) 'declare))
		   (list (pop ,function-body)))))
     ,@body))

(defmacro lambda-async (lambda-list &body body)
  (with-declare-form declare-form body
    `(lambda ,lambda-list
       (make-instance 'promise
		      :thunk (lambda (resolve)
			       ,@declare-form
			       (funcall resolve
					(progn ,@body)))))))

(defmacro defun-async (name lambda-list &body body)
  (with-declare-form declare-form body
    `(defun ,name ,lambda-list
       ,@declare-form
       (make-instance 'promise
		      :thunk (lambda (resolve)
			       (funcall resolve
					(block ,name
					  ,@body)))))))

(defmacro await-let1 ((var promise) &body body)
  `(then ,promise
	 (lambda (,var)
	   ,@body)))

(defmacro await-let* (bindings &body body)
  (unless bindings
    (error "AWAIT-LET* with null BINDINGS is disallowed."))
  `(await-let1 ,(car bindings)
     ,@(if (cdr bindings)
	   (list `(await-let* ,(cdr bindings) ,@body))
	   body)))
	  
