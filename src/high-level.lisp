
(in-package :linear-programming-glpk)


(defstruct glpk-solution
  problem
  solver-mode  ;; :simplex :interior-point or :integer
  objective-value
  var-values ;; padded to be 1-indexed
  var-reduced-costs ;; padded to be 1-indexed
  var-index)

(defmethod linear-programming:solution-problem ((solution glpk-solution))
  (glpk-solution-problem solution))

(defmethod linear-programming:solution-objective-value ((solution glpk-solution))
  (glpk-solution-objective-value solution))

(defmethod linear-programming:solution-variable ((solution glpk-solution) variable)
  (aref (glpk-solution-var-values solution) (gethash variable (glpk-solution-var-index solution))))

(defmethod linear-programming:solution-reduced-cost ((solution glpk-solution) variable)
  (when (eq (glpk-solution-solver-mode solution) :integer)
    (error "GLPK does not provide reduced costs for mixed-integer problems"))
  (aref (glpk-solution-var-reduced-costs solution) (gethash variable (glpk-solution-var-index solution))))

(declaim (inline as-glpk-float glpk-float))
(defun as-glpk-float (num)
  "Converts a number to a type that CFFI can convert to a C double"
  (float num 0.0l0))

(defun glpk-float-type ()
  (type-of 0.0l0))


(defun glpk-solver (problem &key solver-method
                                 (fp-tolerance 1024 fpto-supplied-p)
                                 (enable-presolver t enpre-supplied-p)
                                 (pricing :standard pricing-supplied-p)
                                 (method :primal method-supplied-p)
                                 (ratio-test :standard rate-supplied-p)
                                 (ordering-algorithm :approx-min-degree oal-supplied-p)
                                 (branching-technique :driebeck-tomlin brt-supplied-p)
                                 (backtracking-technique :best-local-bound bat-supplied-p)
                                 (preprocessing-technique :all-levels prepro-supplied-p)
                                 (cut-methods '() cutm-supplied-p)
                                 (time-limit-in-msec nil)
                                 (proxy-time-limit-in-msec nil)
                                 (message-level nil)
                                 &allow-other-keys)
  "Solves the given linear problem using the GLPK library"
  ;; Allocate problem
  (let* ((glpk-ptr (%create-prob))
         ;; Fetch content of problem
         (prob-constraints (linear-programming:problem-constraints problem))
         (prob-vars (linear-programming:problem-vars problem))
         (var-count (length prob-vars))
         (prob-bounds (linear-programming:problem-var-bounds problem))
         (int-vars (linear-programming:problem-integer-vars problem))
         ;; map of variable's to their indices
         (var-index (make-hash-table :size (ceiling (* 5 var-count) 4) :rehash-threshold 1))
         (message-level (ecase message-level
                         ((nil :off) :off)
                         ((:error :warn) :error)
                         ((t :info) :on)
                         (:debug :all)))
         ;; GLPK mode
         (solver-mode (cond
                        (solver-method)
                       ((null int-vars) :simplex)
                       (:integer))))

    ;; Check if solver-mode is known
    (unless (member solver-mode '(:integer :simplex :interior-point))
      (error "Unsupported method ~A" solver-mode))

    ;; Set min or max
    (%set-obj-dir glpk-ptr (if (eq (linear-programming:problem-type problem) 'linear-programming:max) :max :min))

    ;; Set the number of constraints
    (unless (< 0 (length prob-constraints))
      (error "No constraints"))
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
    (%set-obj-name glpk-ptr (string (linear-programming:problem-objective-var problem)))
    (loop :for (var . coef) :in (linear-programming:problem-objective-func problem)
          :for j = (gethash var var-index)
      :do (%set-obj-coef glpk-ptr j (as-glpk-float coef)))

    ;; Set any integer variables
    (loop :for var :in int-vars
          :for j = (gethash var var-index)
      :do (%set-col-kind glpk-ptr j :integer))

    ;; TODO Solve problem
    (ecase solver-mode
      (:simplex
       (when (and solver-method
                  (or brt-supplied-p
                      bat-supplied-p
                      prepro-supplied-p
                      cutm-supplied-p
                      proxy-time-limit-in-msec))
         (error "Unused solver paramaters"))
       (with-foreign-object (ctrl '(:struct simplex-control-parameters))
         (%init-smcp ctrl)
         (setf (foreign-slot-value ctrl '(:struct simplex-control-parameters)
                  'message-level)
               message-level)
         (setf (foreign-slot-value ctrl '(:struct simplex-control-parameters)
                  'enable-presolver)
               (if enable-presolver :on :off))
         (setf (foreign-slot-value ctrl '(:struct simplex-control-parameters)
                  'pricing)
               pricing)
         (setf (foreign-slot-value ctrl '(:struct simplex-control-parameters)
                  'ratio-test)
               ratio-test)
         (setf (foreign-slot-value ctrl '(:struct simplex-control-parameters)
                  'method)
               method)
         (when time-limit-in-msec
           (setf (foreign-slot-value ctrl '(:struct simplex-control-parameters)
                    'time-limit)
                 time-limit-in-msec))
        ;; TODO: support more options
        (let ((result (%simplex glpk-ptr ctrl)))
           (unless (eq result :success)
              (error "Solver failed with state ~A" result)))
        (case (%get-status glpk-ptr)
           ((:no-feasible-solution-exists :infeasible)
            (error 'linear-programming:infeasible-problem-error))
           (:unbounded (error 'linear-programming:unbounded-problem-error)))))
      (:interior-point
       (when (and solver-method
                  (or fpto-supplied-p
                      enpre-supplied-p
                      pricing-supplied-p
                      method-supplied-p
                      rate-supplied-p
                      brt-supplied-p
                      bat-supplied-p
                      prepro-supplied-p
                      cutm-supplied-p
                      time-limit-in-msec
                      proxy-time-limit-in-msec))
         (error "Unused solver paramaters"))
       (with-foreign-object (ctrl '(:struct interior-point-control-parameters))
         (%init-iptcp ctrl)
         (setf (foreign-slot-value ctrl '(:struct interior-point-control-parameters)
                  'message-level)
               message-level)
         (setf (foreign-slot-value ctrl '(:struct interior-point-control-parameters)
                  'ordering-algorithm)
               ordering-algorithm)
         (let ((result (%interior glpk-ptr ctrl)))
            (unless (eq result :success)
               (error "Solver failed with state ~A" result)))
         (case (%ipt-status glpk-ptr)
            ((:no-feasible-solution-exists :infeasible)
             (error 'linear-programming:infeasible-problem-error))
            (:unbounded (error 'linear-programming:unbounded-problem-error)))))
      (:integer
       (when (and solver-method
                  (or fpto-supplied-p
                      enpre-supplied-p
                      pricing-supplied-p
                      method-supplied-p
                      rate-supplied-p
                      oal-supplied-p))
         (error "Unused solver paramaters"))
       (with-foreign-object (ctrl '(:struct integer-control-parameters))
         (%init-iocp ctrl)
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'message-level)
               message-level)
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'branching-technique)
               branching-technique)
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'backtracking-technique)
               backtracking-technique)
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'preprocessing-technique)
               preprocessing-technique)
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'mir-cuts)
               (if (member 'mir cut-methods) :on :off))
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'gomory-cuts)
               (if (member 'gomory cut-methods) :on :off))
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'cover-cuts)
               (if (member 'cover cut-methods) :on :off))
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'cover-cuts)
               (if (member 'cover cut-methods) :on :off))
         (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                  'clique-cuts)
               (if (member 'clique cut-methods) :on :off))
         (if enable-presolver
              (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                       'presolve)
               :on)
              (progn
                 (%simplex glpk-ptr (null-pointer))
                 (case (%get-status glpk-ptr)
                  ((:no-feasible-solution-exists :infeasible)
                   (error 'linear-programming:infeasible-problem-error))
                  (:unbounded (error 'linear-programming:unbounded-problem-error)))))
         (when time-limit-in-msec
           (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                    'time-limit)
                 time-limit-in-msec))
         (when proxy-time-limit-in-msec
           (setf (foreign-slot-value ctrl '(:struct integer-control-parameters)
                    'proxy-time-limit)
                 proxy-time-limit-in-msec))
         ;; TODO: check if all variables are binary, to enable BINARIZE
         ;; TODO: support more options
         (let ((result (%intopt glpk-ptr ctrl)))
            (unless (eq result :success)
               (error "Solver failed with state ~A" result)))
         (case (%mip-status glpk-ptr)
            ((:infeasible :no-feasible-solution-exists)
             (error 'linear-programming:infeasible-problem-error))
            (:unbounded (error 'linear-programming:unbounded-problem-error))))))

    ;; Copy solution to lisp arrays
    ;; GLPK problems can't move threads and lisp's GC gives no guarantee
    (let ((objective-value (ecase solver-mode
                             (:simplex        (%get-obj-val glpk-ptr))
                             (:interior-point (%ipt-obj-val glpk-ptr))
                             (:integer        (%mip-obj-val glpk-ptr))))
          (var-values        (make-array (list (1+ var-count))
                                         :element-type (glpk-float-type)))
          (var-reduced-costs (make-array (list (if (eq solver-mode :integer) 0 (1+ var-count)))
                                         :element-type (glpk-float-type))))

      (ecase solver-mode
        (:simplex
         (loop :for i :from 1 :to var-count
           :do (setf (aref var-values i)       (%get-col-prim glpk-ptr i)
                     (aref var-reduced-costs i) (- (%get-col-dual glpk-ptr i)))))
        (:interior-point
         (loop :for i :from 1 :to var-count
           :do (setf (aref var-values i)       (%ipt-col-prim glpk-ptr i)
                     (aref var-reduced-costs i) (- (%ipt-col-dual glpk-ptr i)))))
        (:integer
         (loop :for i :from 1 :to var-count
           :do (setf (aref var-values i)       (%mip-col-val glpk-ptr i)))))

      ;; Free GLPK's problem object
      (%delete-prob glpk-ptr)

      ;; Return a lisp solution object
      (make-glpk-solution :problem problem
                          :solver-mode solver-mode
                          :objective-value objective-value
                          :var-values var-values
                          :var-reduced-costs var-reduced-costs
                          :var-index var-index))))

(defvar glpk-solver (function glpk-solver))
