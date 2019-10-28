# CL-ASYNC-AWAIT

This library allows you to have async functions similar to those
in JavaScript.

Example:

```
(defun-async :delay example-function (p1 p2)
  (async-handler-case
     (await-let* ((x p1)
                  (y p2))
       (/ x y))
    (division-by-zero (db0) (format *error-output* "Caught error: ~a~%" db0))))

(defun-async promise-value (n) n)

(defvar *p1* (example-function (promise-value 1) (promise-value 2)))
(defvar *p2* (example-function (promise-value 1) (promise-value 0)))

```

Since Common Lisp is not a fully asynchronous language built around PROMISE objects,
it is necessary for the programmer to decide when each PROMISE will be FORCEd.

By default, DEFUN-ASYNC and LAMBDA-ASYNC will create IMMEDIATE-PROMISEs, which
resolve as soon as they are created. Delayed promises can be created with
the :DELAY keyword, whose usage is shown for both DEFUN-ASYNC and
LAMBDA-ASYNC below:

```
(defun :delay function-name lambda-list &body body)
(lambda-async :delay lambda-list &body body)
```

Delayed promises will execute if and only if the FORCE method is invoked on them:

```
(force promise)
```


