# CL-ASYNC-AWAIT

This library allows you to have async functions similar to those
in JavaScript, only these async functions are implemented with
threads instead of an event dispatching mechanism.

Cross-thread error handling is easy using `CL-ASYNC-AWAIT`, since
the `AWAIT` operator propagates errors from the promise, and also
propagates invoked restarts back to the promise.

## Simple example

```
(defun-async my-async-reader (stream)
  (read-byte stream))

(defvar *promise* (my-async-reader *some-stream*))
(defvar *my-byte* (await *promise*))
```

## Usage


```
(lambda-async lambda-list &body body)
```

Creates a `CL:LAMBDA` function that creates a `PROMISE` when FUNCALLed.

```
(defun-async name lambda-list &body body)
```

Like `LAMBDA-ASYNC` but expands to a `CL:DEFUN` form instead of a `CL:LAMBDA` form.

```
(await promise)
```

Wait for a `PROMISE` to resolve to one or more values. If the promise
succeeds, the values will be returned using `CL:VALUES`.

If an error occurs in the `PROMISE` thread and is not handled within the
promise, execution of the PROMISE thread is suspended until the `AWAIT`
method is called.

That error will then be signalled in the thread from which `AWAIT` is called, in
a context where all the same restarts are defined as are defined in the `PROMISE`
thread. If `INVOKE-RESTART` is called with one of the restarts defined in the
`PROMISE` thread, that restart will be invoked in the `PROMISE` thread, and `AWAIT`
will return that restart's value form. 

If the stack frame for the call to `AWAIT` is unwound without invoking a restart,
the `PROMISE` thread will invoke its CL:ABORT restart.

Whether the `PROMISE` succeeds or fails, the result is memoized. Calling `AWAIT` a second time
on the same `PROMISE` will yield the same values.

If an error occurred and `AWAIT` is called a second time, the same error will be signalled, but
the restarts will not be available, since the `PROMISE` thread is expected to be dead as a result
of invoking the `ABORT` restart.

