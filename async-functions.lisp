(in-package :cl-async-await)

(defmacro with-declare-form (var function-body &body body)
  `(let ((,var (if (and (consp (car ,function-body))
			(eq (caar ,function-body) 'declare))
		   (list (pop ,function-body)))))
     ,@body))

(defmacro lambda-async (lambda-list &body body)
  (let ((promise-type (cond ((eq lambda-list :delay)
			     (setf lambda-list (pop body))
			     'promise)
			    ((eq lambda-list :immediate)
			     (setf lambda-list (pop body))
			     'immediate-promise)
			    (t 'parallel-promise))))
    (with-declare-form declare-form body
      `(lambda ,lambda-list
	 (make-instance ',promise-type
			:thunk (lambda (resolve)
				 ,@declare-form
				 (funcall resolve
					  (progn ,@body))))))))

(defmacro defun-async (name lambda-list &body body)
  (let ((promise-type (cond ((eq name :delay)
			     (setf name lambda-list)
			     (setf lambda-list (pop body))
			     'promise)
			    ((eq name :immediate)
			     (setf name lambda-list)
			     (setf lambda-list (pop body))
			     'immediate-promise)
			    (t 'parallel-promise))))
    (with-declare-form declare-form body
      `(defun ,name ,lambda-list
	 ,@declare-form
	 (make-instance ',promise-type
			:thunk (lambda (resolve)
				 (funcall resolve
					  (block ,name
					    ,@body))))))))

(defmacro await-let1 ((var promise) &body body)
  `(then ,promise
	 (lambda (,var)
	   ,@body)))

(defmacro await-let* (bindings &body body)
  "Sequentially bind variables to different promises."
  (unless bindings
    (error "AWAIT-LET* with null BINDINGS is disallowed."))
  `(await-let1 ,(car bindings)
     ,@(if (cdr bindings)
	   (list `(await-let* ,(cdr bindings) ,@body))
	   body)))

(defmacro await-multiple-value-bind ((lambda-list promise) &body body)
  "Bind the multiple values returned by the PROMISE to a lambda-list."
  `(then ,promise
	 (lambda ,lambda-list ,@body)))

(defmacro async-handler-case (form &body cases)
  "Behaves just like CL:HANDLER-CASE, except if the FORM evals to a CL-ASYNC-AWAIT:PROMISE,
it will attach error handlers to the promise and then return it."
  (let ((promise (gensym)))
    `(handler-case
	 (let ((,promise ,form))
	   ,@(loop for (condition-type lambda-list . body) in cases
		collect `(catch-exception ,promise ',condition-type
					  (lambda ,lambda-list ,@body)))
	   ,promise)
       ,@cases)))
	   
