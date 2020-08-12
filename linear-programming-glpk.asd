

(asdf:defsystem :linear-programming-glpk
  :name "linear-programming-glpk"
  :description "A backend for linear-programming using GLPK"
  :author "Nei Lindquist <NeilLindquist5@gmail.com>"
  :license "GPL 3.0"
  :version "0.0.0"
  :bug-tracker "https://github.com/neil-lindquist/linear-programming-gplk/issues"
  :mailto "NeilLindquist5@gmail.com"
  :source-control (:git "https://github.com/neil-lindquist/linear-programming-gplk.git")

  :depends-on (:cffi
               :trivial-garbage ;; for finalizer to destroy glpk problem objects
               :linear-programming)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "ffi")
               (:file "high-level")))

(defsystem "linear-programming-glpk/test"
  :description "The tests for the linear-programming-glpk package"
  :author "Neil Lindquist <NeilLindquist5@gmail.com>"
  :licence "GPL 3.0"
  :depends-on (:linear-programming-glpk
               :fiveam)
  :serial t
  :pathname "src"
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call '#:fiveam '#:run! (intern "LINEAR-PROGRAMMING-GLPK" :linear-programming-glpk/test))))
