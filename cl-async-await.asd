(asdf:defsystem :cl-async-await
    :author ("Jeremy Phelps")
    :version "1"
    :license "AGPLv3"
    :description "An implementation of async/await for Common Lisp"
    :depends-on (:closer-mop)
    :components
    ((:file "promise")))
