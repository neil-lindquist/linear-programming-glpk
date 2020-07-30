
(in-package :linear-programming-glpk)


(defstruct glpk-solution
  problem
  ptr
  solver-mode  ;; :simplex :interior-point or :integer
  var-index)

(defmethod lp:solution-problem ((solution glpk-solution))
  (glpk-solution-problem solution))

(defmethod lp:solution-objective-value ((solution glpk-solution))
  (ecase (glpk-solution-solver-mode solution)
    (:simplex        (%get-obj-val (glpk-solution-ptr solution)))
    (:interior-point (%ipt-obj-val (glpk-solution-ptr solution)))
    (:integer        (%mip-obj-val (glpk-solution-ptr solution)))))

(defmethod lp:solution-variable ((solution glpk-solution) variable)
  (ecase (glpk-solution-solver-mode solution)
    (:simplex        (%get-col-prim (glpk-solution-ptr solution) (gethash variable (glpk-solution-var-index solution))))
    (:interior-point (%ipt-col-prim (glpk-solution-ptr solution) (gethash variable (glpk-solution-var-index solution))))
    (:integer        (%mip-col-val  (glpk-solution-ptr solution) (gethash variable (glpk-solution-var-index solution))))))

(defmethod lp:solution-reduced-cost ((solution glpk-solution) variable)
  (ecase (glpk-solution-solver-mode solution)
    (:simplex        (%get-col-dual (glpk-solution-ptr solution) (gethash variable (glpk-solution-var-index solution))))
    (:interior-point (%ipt-col-dual (glpk-solution-ptr solution) (gethash variable (glpk-solution-var-index solution))))
    (:integer        (error "GLPK does not provide reduced costs for mixed-integer problems"))))


(declaim (inline as-glpk-float))
(defun as-glpk-float (num)
  "Converts a number to a type that CFFI can convert to a C double"
  (float num 0.0l0))


(defun glpk-solver (problem &rest args)
  "Solves the given linear problem using the GLPK library"

  ;; Allocate problem
  (let* ((glpk-ptr (%create-prob))
         ;; Fetch content of problem
         (prob-constraints (lp:problem-constraints problem))
         (prob-vars (lp:problem-vars problem))
         (prob-bounds (lp:problem-var-bounds problem))
         (int-vars (lp:problem-integer-vars problem))
         ;; map of variable's to their indices
         (var-index (make-hash-table :size (ceiling (* 5 (length prob-vars)) 4) :rehash-threshold 1))
         ;; GLPK mode
         (solver-mode (if (null int-vars) :simplex :integer)))

    ;; Set min or max
    (%set-obj-dir glpk-ptr (if (eq (lp:problem-type problem) 'lp:max) :max :min))

    ;; Set the number of constraints
    (%add-rows glpk-ptr (length prob-constraints))

    ;; Set the number of variables, their names, and their bounds
    (%add-cols glpk-ptr (length prob-vars))
    (loop :for var :across prob-vars
          :for j from 1
          :for bound = (or (rest (assoc var prob-bounds))
                           '(0 . nil))
          :for glpk-type = (cond
                             ((equal bound '(nil . nil)) :free)
                             ((equal (car bound) nil) :upper-bound)
                             ((equal (cdr bound) nil) :lower-bound)
                             ((/= (car bound) (cdr bound)) :double-bound)
                             (t :fixed))
      :do (setf (gethash var var-index) j)
      :do (%set-col-name glpk-ptr j (string var))
      :do (%set-col-bnds glpk-ptr j glpk-type (as-glpk-float (or (car bound) 0)) (as-glpk-float (or (cdr bound) 0))))

    ;; Fill the rows of the constraint matrix
    (loop :for constraint :in prob-constraints
          :for i :from 1
          :for glpk-type = (ecase (first constraint)
                              (>= :lower-bound)
                              (<= :upper-bound)
                              (=  :fixed))
       :do (let* ((num-entries (length (second constraint)))
                  (inds (foreign-alloc :int :count (1+ num-entries)))
                  (vals (foreign-alloc :double :count (1+ num-entries))))

             (%set-row-bnds glpk-ptr i glpk-type (as-glpk-float (third constraint)) (as-glpk-float (third constraint)))

             (loop :for entry :in (second constraint)
                   :for var = (car entry)
                   :for coef = (cdr entry)
                   :for j = (gethash var var-index)
                   :for k :from 1
               :do (setf (mem-aref inds :int k) j)
               :do (setf (mem-aref vals :double k) (as-glpk-float coef)))
             (%set-mat-row glpk-ptr i num-entries inds vals)))

    ;; Setup the objective function
    (%set-obj-name glpk-ptr (string (lp:problem-objective-var problem)))
    (loop :for (var . coef) :in (lp:problem-objective-func problem)
          :for j = (gethash var var-index)
      :do (%set-obj-coef glpk-ptr j (as-glpk-float coef)))

    ;; Set any integer variables
    (loop :for var :in int-vars
          :for j = (gethash var var-index)
      :do (%set-col-kind glpk-ptr j :integer))

    ;; TODO Solve problem
    (ecase solver-mode
      (:simplex (%simplex glpk-ptr (null-pointer)))
      (:integer (%intopt glpk-ptr)))

    ;; Create solution object and return
    (let ((sol (make-glpk-solution :problem problem
                                   :ptr glpk-ptr
                                   :solver-mode solver-mode
                                   :var-index var-index)))
      (trivial-garbage:finalize sol (lambda () (%delete-prob glpk-ptr)))
      sol)))

(defvar glpk-solver (function glpk-solver))
