

(asdf:defsystem :linear-programming-glpk
  :name "linear-programming-glpk"
  :description "A backend for linear-programming using GLPK"
  :author "Nei Lindquist <NeilLindquist5@gmail.com>"
  :license "GPL 3.0"
  :version "0.0.0"
  :bug-tracker "https://github.com/neil-lindquist/linear-programming-gplk/issues"
  :mailto "NeilLindquist5@gmail.com"
  :source-control (:git "https://github.com/neil-lindquist/linear-programming-gplk.git")

  :depends-on (:cffi :linear-programming)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "ffi")))
