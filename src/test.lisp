
(defpackage :linear-programming-glpk/test
  (:use :cl
        :fiveam)
  (:local-nicknames (:lp :linear-programming)
                    (:glpk :linear-programming-glpk))
  (:export #:linear-programming-glpk))

(in-package :linear-programming-glpk/test)

(def-suite* linear-programming-glpk
  :description "The test suite for the GLPK backend tests")

;; Use the glpk backend
(setf lp:*solver* glpk:glpk-solver)

(test :general-api
  (is (eq glpk:glpk-solver (function glpk:glpk-solver)))
  (is (eq lp:*solver* glpk:glpk-solver)))


(test :simplex
  (let* ((problem (lp:make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                          (<= (+ (* 2 x) y) 8)
                                          (<= (+ y z) 7)))
         (solution (lp:solve-problem problem)))
    (is (eq problem (lp:solution-problem solution)))
    (is (= 57/2 (lp:solution-objective-value solution)))
    (is (= 1/2 (lp:solution-variable solution 'x)))
    (is (= 7 (lp:solution-variable solution 'y)))
    (is (= 0 (lp:solution-variable solution 'z)))
    (is (= 0 (lp:solution-reduced-cost solution 'x)))
    (is (= 0 (lp:solution-reduced-cost solution 'y)))
    (is (= 1/2 (lp:solution-reduced-cost solution 'z)))))


(test :integer
  ; Rock of Gibralter problem
  (let* ((problem (lp:make-linear-problem (max (+ (* 240 x) (* 120 y)))
                                       (<= (+ x y) 5)
                                       (<= (+ (* -1 x) y) 0)
                                       (<= (+ (* 6 x) (* 2 y)) 21)
                                       (integer x y)))
         (solution (lp:solve-problem problem)))
    (is (eq problem (lp:solution-problem solution)))
    (is (= 840 (lp:solution-objective-value solution)))
    (is (= 3 (lp:solution-variable solution 'x)))
    (is (= 1 (lp:solution-variable solution 'y)))))
