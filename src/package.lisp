
(defpackage :linear-programming-glpk
  (:use #:cl
        #:cffi)
  (:export #:glpk-solver
           #:exit-code
           #:status
           #:glpk-solver-error
           #:premature-solution-error))
