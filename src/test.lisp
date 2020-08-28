
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


(test :glpk
  ;; GLPK example from the manual (p. 12f)
  (let* ((problem (lp:make-linear-problem
		   (max (+ (* 10 x1) (* 6 x2) (* 4 x3)))
		   (<= 0 x1) (<= 0 x2) (<= 0 x3)
		   (<= (+ x1 x2 x3)			100)
		   (<= (+ (* 10 x1) (* 4 x2) (* 5 x3))	600)
		   (<= (+ (*  2 x1) (* 2 x2) (* 6 x3))	300)))
         (solution (lp:solve-problem problem :method :simplex))
	 (eps 1d-10))
    (is (eq problem (lp:solution-problem solution)))
    (is (<= (- 2200/3 eps) (lp:solution-objective-value solution) (+ 2200/3 eps)))
    (is (<= (- 100/3 eps) (lp:solution-variable solution 'x1) (+ 100/3 eps)))
    (is (<= (- 200/3 eps) (lp:solution-variable solution 'x2) (+ 200/3 eps)))
    (is (= 0 (lp:solution-variable solution 'x3)))))


(test :large
  (let* ((vars (loop :repeat 10000 :collect (gensym)))
	 (problem (lp:parse-linear-problem
		   `(min (+ . ,vars))
		   (loop :for var1 :in vars
			 :for var2 :in (rest vars)
			 :collect (list '= var1 var2))))
	 (solution (lp:solve-problem problem)))
    (dolist (var vars)
      (is (= 0 (lp:solution-variable solution var))))))


(test :infeasible
  (fiveam:signals (error lp:infeasible-problem-error)
    (lp:with-solved-problem ((max x)
			     (< x y)
			     (< y x))
      (error "Should not work"))))


(test :unbounded
  (fiveam:signals (error lp:infeasible-problem-error)
    (lp:with-solved-problem ((max x)
			     (< 0 x)
			     (< y x)
			     (< 0 y))
      (error "Should not work"))))


(test :no-constraints
  (fiveam:signals (error)
    (lp:with-solved-problem ((max x))
      (error "Should not work"))))
