
(defpackage :linear-programming-glpk/test
  (:use :cl
        :fiveam)
  (:export #:linear-programming-glpk))

(in-package :linear-programming-glpk/test)

(def-suite* linear-programming-glpk
  :description "The test suite for the GLPK backend tests")

;; Use the glpk backend
(setf linear-programming:*solver* linear-programming-glpk:glpk-solver)

(test :general-api
  (is (eq linear-programming-glpk:glpk-solver (function linear-programming-glpk:glpk-solver)))
  (is (eq linear-programming:*solver* linear-programming-glpk:glpk-solver)))


(test :simplex
  (let* ((problem (linear-programming:make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                          (<= (+ (* 2 x) y) 8)
                                          (<= (+ y z) 7)))
         (solution (linear-programming:solve-problem problem)))
    (is (eq problem (linear-programming:solution-problem solution)))
    (is (= 57/2 (linear-programming:solution-objective-value solution)))
    (is (= 1/2 (linear-programming:solution-variable solution 'x)))
    (is (= 7 (linear-programming:solution-variable solution 'y)))
    (is (= 0 (linear-programming:solution-variable solution 'z)))
    (is (= 0 (linear-programming:solution-reduced-cost solution 'x)))
    (is (= 0 (linear-programming:solution-reduced-cost solution 'y)))
    (is (= 1/2 (linear-programming:solution-reduced-cost solution 'z)))))


(test :integer
  ; Rock of Gibralter problem
  (let* ((problem (linear-programming:make-linear-problem (max (+ (* 240 x) (* 120 y)))
                                          (<= (+ x y) 5)
                                          (<= (+ (* -1 x) y) 0)
                                          (<= (+ (* 6 x) (* 2 y)) 21)
                                          (integer x y)))
         (solution (linear-programming:solve-problem problem)))
    (is (eq problem (linear-programming:solution-problem solution)))
    (is (= 840 (linear-programming:solution-objective-value solution)))
    (is (= 3 (linear-programming:solution-variable solution 'x)))
    (is (= 1 (linear-programming:solution-variable solution 'y)))))


(test :glpk
  ;; GLPK example from the manual (p. 12f)
  (let* ((problem (linear-programming:make-linear-problem
                   (max (+ (* 10 x1) (* 6 x2) (* 4 x3)))
                   (<= (+ x1 x2 x3)                     100)
                   (<= (+ (* 10 x1) (* 4 x2) (* 5 x3))  600)
                   (<= (+ (*  2 x1) (* 2 x2) (* 6 x3))  300)))
         (solution (linear-programming:solve-problem problem :method :simplex))
         (eps 1d-10))
    (is (eq problem (linear-programming:solution-problem solution)))
    (is (<= (- 2200/3 eps) (linear-programming:solution-objective-value solution) (+ 2200/3 eps)))
    (is (<= (- 100/3 eps) (linear-programming:solution-variable solution 'x1) (+ 100/3 eps)))
    (is (<= (- 200/3 eps) (linear-programming:solution-variable solution 'x2) (+ 200/3 eps)))
    (is (= 0 (linear-programming:solution-variable solution 'x3)))))


(test :large
  (let* ((vars (loop :repeat 10000 :collect (gensym)))
         (problem (linear-programming:parse-linear-problem
                   `(min (+ . ,vars))
                   (loop :for var1 :in vars
                     :for var2 :in (rest vars)
                     :collect (list '= var1 var2))))
         (solution (linear-programming:solve-problem problem)))
    (dolist (var vars)
      (is (= 0 (linear-programming:solution-variable solution var))))))


(test :infeasible
  (fiveam:signals (error linear-programming:infeasible-problem-error)
    (linear-programming:with-solved-problem ((max x))
                             (< x y)
                             (< y x)
      (error "Should not work"))))


(test :unbounded
  (fiveam:signals (error linear-programming:infeasible-problem-error)
    (linear-programming:with-solved-problem ((max x))
                             (< 0 x)
                             (< y x)
                             (< 0 y)
      (error "Should not work"))))


(test :no-constraints
  (fiveam:signals (error)
    (linear-programming:with-solved-problem ((max x))
      (error "Should not work"))))
