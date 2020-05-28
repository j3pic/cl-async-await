(asdf:defsystem :cl-async-await
    :author ("Jeremy Phelps")
    :version "1"
    :license "AGPLv3"
    :description "An implementation of async/await for Common Lisp"
    :depends-on (:closer-mop :bordeaux-threads)
    :components
    ((:file "package")
     (:file "utils" :depends-on ("package"))
     (:file "async-functions" :depends-on ("package" "promise" "utils"))
     (:file "promise" :depends-on ("package" "utils"))))
