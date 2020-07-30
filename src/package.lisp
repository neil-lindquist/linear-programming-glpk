
(defpackage :linear-programming-glpk
  (:use #:cl
        #:cffi)
  (:local-nicknames (:lp :linear-programming))
  (:export #:glpk-solver))
