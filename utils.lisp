(in-package :cl-async-await)

(setf (symbol-function 'assoc)
      #'cl:assoc)

(defmacro if-let (var cond if-true &optional if-false)
  `(let ((,var ,cond))
     (if ,var ,if-true ,if-false)))

(defmacro aif (cond if-true &optional if-false)
  `(if-let it ,cond ,if-true ,if-false))

(defun (setf assoc) (new-value key alist)
  (aif (assoc key alist)
       (progn
	 (setf (cdr it) new-value)
	 it)
       (push (cons key new-value) alist)))
