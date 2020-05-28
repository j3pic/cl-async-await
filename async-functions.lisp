(in-package :cl-async-await)

(defmacro with-declare-form (var function-body &body body)
  `(let ((,var (if (and (consp (car ,function-body))
			(eq (caar ,function-body) 'declare))
		   (list (pop ,function-body)))))
     ,@body))

(defmacro lambda-async (lambda-list &body body)
  "Usage:

(lambda-async lambda-list &body body) or
(lambda-async :kw lambda-list &body body)

Expands to a CL:LAMBDA that returns a PROMISE when FUNCALLed.

The PROMISE resolves to whatever the BODY returns.

The :KW parameter, if provided, must be replaced with either :DELAY or
:IMMEDIATE. If :DELAY is given, the lambda will return a PROMISE
object which won't execute until its value is requested (ie, until it is
FORCEd).

If :IMMEDIATE is given, the BODY is executed synchronously, and an
already-resolved promise will be returned.

If no :KW argument is given, then a PARALLEL-PROMISE is created. Its
code will execute immediately in a new thread.
"
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
				 (apply resolve
					(multiple-value-list 
					 (progn ,@body)))))))))

(defmacro defun-async (name lambda-list &body body)
  "Just like LAMBDA-ASYNC, except it expands to a CL:DEFUN form instead of CL:LAMBDA."
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
				 (apply resolve
					(multiple-value-list
					 (block ,name
					   ,@body)))))))))

(defmacro await-let1 ((var promise) &body body)
  (alexandria:with-gensyms (ignored-values)
    `(then ,promise
	   (lambda (,var &rest ,ignored-values)
	     (declare (ignore ,ignored-values))
	     ,@body))))

(defmacro await-let* (bindings &body body)
  "Sequentially bind variables to different promises."
  (unless bindings
    (error "AWAIT-LET* with null BINDINGS is disallowed."))
  `(await-let1 ,(car bindings)
     ,@(if (cdr bindings)
	   (list `(await-let* ,(cdr bindings) ,@body))
	   body)))

(defmacro await-multiple-value-bind (lambda-list promise &body body)
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
	   
