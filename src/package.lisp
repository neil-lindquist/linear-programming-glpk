
(defpackage :linear-programming-glpk
  (:use #:cl
        #:cffi)
  (:export #:glpk-solver
           #:premature-exit-error))
