;; Code heavily based off https://github.com/sellout/cl-glpk
;; Used under the GPL 3.0 and BSD sans advertising licenses

(in-package :linear-programming-glpk)

(define-foreign-library libglpk
  (t (:default "libglpk")))

(use-foreign-library libglpk)

(defcenum problem-class
  (:lp 100)      ;; linear programming
  (:mip 101))    ;; mixed integer programming

(defcenum optimization-direction-flag
  (:min 1)
  (:max 2))

;; type of auxiliary/structural variable
(defcenum variable-type
  (:free 1)           ;; /* free variable */
  (:lower-bound 2)    ;; /* variable with lower bound */
  (:upper-bound 3)  ;; /* variable with upper bound */
  (:double-bound 4)  ;; /* double-bounded variable */
  (:fixed 5))    ;; /* fixed variable */

(defcenum basis-status
  (:undefined 130)
  (:valid 131))

(defcenum primal-basic-solution-status
  (:undefined 1)
  :feasible
  :infeasible
  :no-feasible-solution-exists
  :optimal
  :unbounded)

(defcenum dual-basic-solution-status
  (:undefined 136)
  :feasible
  :infeasible
  :no-feasible-solution-exists)

(defcenum variable-status
  (:basic 1)
  :non-basic-on-lower-bound
  :non-basic-on-upper-bound
  :non-basic-free
  :non-basic-fixed)

(defcenum interior-point-solution-status
  (:undefined 150)
  :optimal)

(defcenum structural-variable-kind
  (:continuous 160)
  :integer)

(defcenum integer-solution-status
  (:undefined 170)
  :optimal
  :feasible
  :no-integer-solution-exists)

(defcenum lpx-status
  (:optimal 0)
  :feasible
  :infeasible
  :no-feasible
  :unbounded
  :undefined)

(defcenum solver-exit-code
  (:ok 0)
  :empty
  :bad-basis
  :infeasible
  :fault
  :objective-lower-limit-reached
  :objective-upper-limit-reached
  :iteration-limit-exhausted
  :time-limit-exhausted
  :no-feasible-solution
  :numerical-instability
  :sing
  :no-convergence
  :no-primal-feasible-solution
  :no-dual-feasible-solution)


;; todo: lispify names; in particular, use longer names
(defcenum control-parameter
  (:msglev 300) ;; /* lp->msg_lev */
  (:scale 301)  ;; /* lp->scale */
  (:dual 302)  ;; /* lp->dual */
  (:price 303)  ;; /* lp->price */
  (:relax 304)  ;; /* lp->relax */
  (:tolbnd 305) ;; /* lp->tol_bnd */
  (:toldj 306)  ;; /* lp->tol_dj */
  (:tolpiv 307) ;; /* lp->tol_piv */
  (:round 308)  ;; /* lp->round */
  (:objll 309)  ;; /* lp->obj_ll */
  (:objul 310)  ;; /* lp->obj_ul */
  (:itlim 311)  ;; /* lp->it_lim */
  (:itcnt 312)  ;; /* lp->it_cnt */
  (:tmlim 313)  ;; /* lp->tm_lim */
  (:outfrq 314) ;; /* lp->out_frq */
  (:outdly 315) ;; /* lp->out_dly */
  (:branch 316) ;; /* lp->branch */
  (:btrack 317) ;; /* lp->btrack */
  (:tolint 318) ;; /* lp->tol_int */
  (:tolobj 319) ;; /* lp->tol_obj */
  (:mpsinfo 320) ;; /* lp->mps_info */
  (:mpsobj 321)   ;; /* lp->mps_obj */
  (:mpsorig 322) ;; /* lp->mps_orig */
  (:mpswide 323) ;; /* lp->mps_wide */
  (:mpsfree 324) ;; /* lp->mps_free */
  (:mpsskip 325) ;; /* lp->mps_skip */
  (:lptorig 326) ;; /* lp->lpt_orig */
  (:presol 327)   ;; /* lp->presol */
  (:binarize 328) ;; /* lp->binarize */
  (:usecuts 329)) ;; /* lp->use_cuts */

(defcenum cut-types
  (:cover     3)  ;/* mixed cover cuts */
  (:clique    4)  ;/* clique cuts */
  (:gomory    1)  ;/* Gomory's mixed integer cuts */
  (:all       0)) ; /* all cuts */

(defcfun ("glp_create_prob" %create-prob) :pointer)
;; /* create problem object */

(defcfun ("glp_set_prob_name" %set-prob-name)
    :void
  (problem :pointer)
  (name :string))
;;/* assign (change) problem name */

(defcfun ("glp_set_obj_name" %set-obj-name)
    :void
  (problem :pointer)
  (name :string))
;; /* assign (change) objective function name */

(defcfun ("glp_set_obj_dir" %set-obj-dir)
    :void
  (problem :pointer)
  (direction optimization-direction-flag))
;;/* set (change) optimization direction flag */

(defcfun ("glp_add_rows" %add-rows)
    :int
  (problem :pointer)
  (new-rows :int))
;;/* add new rows to problem object */

(defcfun ("glp_add_cols" %add-cols)
    :int
  (problem :pointer)
  (new-columns :int))
;;/* add new columns to problem object */

(defcfun ("glp_set_row_name" %set-row-name)
    :void
  (problem :pointer)
  (index :int)
  (name :string))
;;/* assign (change) row name */

(defcfun ("glp_set_col_name" %set-col-name)
    :void
  (problem :pointer)
  (index :int)
  (name :string))
;;/* assign (change) column name */

(defcfun ("glp_set_row_bnds" %set-row-bnds)
    :void
  (problem :pointer)
  (index :int)
  (type variable-type)
  (lower-bound :double)
  (upper-bound :double))
;;/* set (change) row bounds */

(defcfun ("glp_set_col_bnds" %set-col-bnds)
    :void
  (problem :pointer)
  (index :int)
  (type variable-type)
  (lower-bound :double)
  (upper-bound :double))
;; /* set (change) column bounds */

(defcfun ("glp_set_obj_coef" %set-obj-coef)
    :void
  (problem :pointer)
  (index :int)
  (coefficient :double))
;; /* set (change) obj. coefficient or constant term */

(defcfun ("glp_set_mat_row" %set-mat-row)
    :void
  (problem :pointer)
  (row-index :int)
  (length :int)
  (column-indices :pointer) ;; int array
  (values :pointer)) ;; double array
;; set (replace) row of the constraint matrix

(defcfun ("glp_set_mat_col" %set-mat-col)
    :void
  (problem :pointer)
  (column-index :int)
  (length :int)
  (row-indices :pointer) ;; int array
  (values :pointer)) ;; double array
;; /* set (replace) column of the constraint matrix */


(defcfun ("glp_load_matrix" %load-matrix)
    :void
  (problem :pointer)
  (number-of-entries :int)
  (rows :pointer)
  (columns :pointer)
  (values :pointer))
;; void lpx_load_matrix(LPX *lp, int ne, int ia[], int ja[], double ar[]);
;; /* load (replace) the whole constraint matrix */

(defcfun ("glp_del_rows" %del-rows)
    :void
  (problem :pointer)
  (number-of-rows :int)
  (rows :pointer))
;; /* delete specified rows from problem object */

(defcfun ("glp_del_cols" %del-cols)
    :void
  (problem :pointer)
  (number-of-columns :int)
  (columns :pointer))
;; /* delete specified columns from problem object */

(defcfun ("glp_delete_prob" %delete-prob)
    :void
  (problem :pointer))
;; /* delete problem object */

(defcfun ("glp_get_prob_name" %get-prob-name)
    :string
  (problem :pointer))
;; /* retrieve problem name */

(defcfun ("glp_get_obj_name" %get-obj-name)
    :string
  (problem :pointer))
;; /* retrieve objective function name */

(defcfun ("glp_get_obj_dir" %get-obj-dir)
    optimization-direction-flag
  (problem :pointer))
;;/* retrieve optimization direction flag */

(defcfun ("glp_get_num_rows" %get-num-rows)
    :int
  (problem :pointer))
;;/* retrieve number of rows */

(defcfun ("glp_get_num_cols" %get-num-cols)
    :int
  (problem :pointer))
;;/* retrieve number of columns */

(defcfun ("glp_get_row_name" %get-row-name)
    :string
  (problem :pointer)
  (index :int))
;;/* retrieve row name */

(defcfun ("glp_get_col_name" %get-col-name)
    :string
  (problem :pointer)
  (index :int))
;;/* retrieve column name */

(defcfun ("glp_get_row_type" %get-row-type)
    variable-type
  (problem :pointer)
  (index :int))
;;/* retrieve row type */

(defcfun ("glp_get_row_lb" %get-row-lb)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row lower bound */

(defcfun ("glp_get_row_ub" %get-row-ub)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row upper bound */

#|
(defcfun ("glp_get_row_bnds" %get-row-bnds))   /* obsolete */
void lpx_get_row_bnds(LPX *lp, int i, int *typx, double *lb,
                      double *ub);
/* obtain row bounds */
|#

(defcfun ("glp_get_col_type" %get-col-type)
    variable-type
  (problem :pointer)
  (index :int))
;; /* retrieve column type */

(defcfun ("glp_get_col_lb" %get-col-lb)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column lower bound */

(defcfun ("glp_get_col_ub" %get-col-ub)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column upper bound */

#|
(defcfun ("glp_get_col_bnds" %get-col-bnds))   /* obsolete */
void lpx_get_col_bnds(LPX *lp, int j, int *typx, double *lb,
                      double *ub);
/* obtain column bounds */
|#

(defcfun ("glp_get_obj_coef" %get-obj-coef)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve obj. coefficient or constant term */

(defcfun ("glp_get_num_nz" %get-num-nz)
    :int
  (problem :pointer))
;;/* retrieve number of constraint coefficients */

(defcfun ("glp_get_mat_row" %get-mat-row)
    :int
  (problem :pointer)
  (index :int)
  (indices :pointer)  ; array of int
  (values :pointer))  ; array of double
;;/* retrieve row of the constraint matrix */

(defcfun ("glp_get_mat_col" %get-mat-col)
    :int
  (problem :pointer)
  (index :int)
  (indices :pointer) ; array of int
  (values :double))  ; array of double
;;/* retrieve column of the constraint matrix */

(defcfun ("glp_create_index" %create-index)
    :void
  (problem :pointer))

(defcfun ("glp_find_row" %find-row)
    :int
  (problem :pointer)
  (name :string))

(defcfun ("glp_find_col" %find-col)
    :int
  (problem :pointer)
  (name :string))

(defcfun ("glp_delete_index" %delete-index)
    :void
  (problem :pointer))

(defcfun ("glp_scale_prob" %scale-prob)
    :void
  (problem :pointer))
;;/* scale problem data */

(defcfun ("glp_unscale_prob" %unscale-prob)
    :void
  (problem :pointer))
;;/* unscale problem data */

(defcfun ("glp_std_basis" %std-basis)
    :void
  (problem :pointer))
;;/* construct standard initial LP basis */

(defcfun ("glp_adv_basis" %adv-basis)
    :void
  (problem :pointer))
;;/* construct advanced initial LP basis */

(defcfun ("glp_cpx_basis" %cpx-basis)
    :void
  (problem :pointer))
;;/* construct Bixby's initial LP basis */

(defcfun ("glp_set_row_stat" %set-row-stat)
    :void
  (problem :pointer)
  (index :int)
  (status variable-status))

(defcfun ("glp_set_col_stat" %set-col-stat)
    :void
  (problem :pointer)
  (index :int)
  (status variable-status))

(defcfun ("glp_simplex" %simplex)
    solver-exit-code
  (problem :pointer)
  (params :pointer))

(defcfun ("glp_exact" %exact)
    solver-exit-code
  (problem :pointer)
  (params :pointer))
;;/* easy-to-use driver to the exact simplex method */

(defcfun ("glp_get_status" %get-status)
    lpx-status
  (problem :pointer))
;;/* retrieve generic status of basic solution */

(defcfun ("glp_get_prim_stat" %get-prim-stat)
    primal-basic-solution-status
  (problem :pointer))
;;/* retrieve primal status of basic solution */

(defcfun ("glp_get_dual_stat" %get-dual-stat)
    dual-basic-solution-status
  (problem :pointer))
;;/* retrieve dual status of basic solution */

(defcfun ("glp_get_obj_val" %get-obj-val)
    :double
  (problem :pointer))
;; /* retrieve objective value (basic solution) */

(defcfun ("glp_get_row_stat" %get-row-stat)
    variable-status
  (problem :pointer)
  (index :int))
;;/* retrieve row status (basic solution) */

(defcfun ("glp_get_row_prim" %get-row-prim)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row primal value (basic solution) */

(defcfun ("glp_get_row_dual" %get-row-dual)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row dual value (basic solution) */

#|
(defcfun ("glp_get_row_info" %get-row-info))   /* obsolete */
void lpx_get_row_info(LPX *lp, int i, int *tagx, double *vx,
                      double *dx);
/* obtain row solution information */
|#

(defcfun ("glp_get_col_stat" %get-col-stat)
    variable-status
  (problem :pointer)
  (index :int))
;;/* retrieve column status (basic solution) */

(defcfun ("glp_get_col_prim" %get-col-prim)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column primal value (basic solution) */

(defcfun ("glp_get_col_dual" %get-col-dual)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column dual value (basic solution) */

#|
(defcfun ("glp_get_col_info" %get-col-info))   /* obsolete */
void lpx_get_col_info(LPX *lp, int j, int *tagx, double *vx,
                      double *dx);
/* obtain column solution information */
|#

;; TODO: This function returns the index of some non-basic variable
;; which causes primal unboundedness, and 0 if it doesn't know
;; one. We'd probably prefer to return :UNKNOWN in the latter case.
(defcfun ("glp_get_ray_info" %get-ray-info)
    :int
  (problem :pointer))
;;/* determine what causes primal unboundness */

(defcfun ("glp_warm_up" %warm-up)
    solver-exit-code
  (problem :pointer))
;;/* "warm up" LP basis */

(defcfun ("glp_interior" %interior)
    solver-exit-code
  (problem :pointer))
;;/* easy-to-use driver to the interior point method */

(defcfun ("glp_ipt_status" %ipt-status)
    interior-point-solution-status
  (problem :pointer))
;;/* retrieve status of interior-point solution */

(defcfun ("glp_ipt_obj_val" %ipt-obj-val)
    :double
  (problem :pointer))
;;/* retrieve objective value (interior point) */

(defcfun ("glp_ipt_row_prim" %ipt-row-prim)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row primal value (interior point) */

(defcfun ("glp_ipt_row_dual" %ipt-row-dual)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row dual value (interior point) */

(defcfun ("glp_ipt_col_prim" %ipt-col-prim)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column primal value (interior point) */

(defcfun ("glp_ipt_col_dual" %ipt-col-dual)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column dual value (interior point) */

;; TODO: Figure out what values for class are valid.
(defcfun ("glp_set_class" %set-class)
    :void
  (problem :pointer)
  (class :int))
;;/* set (change) problem class */

;; TODO: See ..._set_class
(defcfun ("glp_get_class" %get-class)
    :int
  (problem :pointer))
;;/* retrieve problem class */

(defcfun ("glp_set_col_kind" %set-col-kind)
    :void
  (problem :pointer)
  (index :int)
  (kind structural-variable-kind))
;;/* set (change) column kind */

(defcfun ("glp_get_col_kind" %get-col-kind)
    structural-variable-kind
  (problem :pointer)
  (index :int))
;;/* retrieve column kind */

(defcfun ("glp_get_num_int" %get-num-int)
    :int
  (problem :pointer))
;;/* retrieve number of integer columns */

(defcfun ("glp_get_num_bin" %get-num-bin)
    :int
  (problem :pointer))
;;/* retrieve number of binary columns */

(defcfun ("glp_integer" %integer)
    :int
  (problem :pointer))
;;/* easy-to-use driver to the branch-and-bound method */

(defcfun ("glp_intopt" %intopt)
    solver-exit-code
  (problem :pointer))
;;/* easy-to-use driver to the branch-and-bound method */

(defcfun ("glp_mip_status" %mip-status)
    integer-solution-status
  (problem :pointer))
;;/* retrieve status of MIP solution */

(defcfun ("glp_mip_obj_val" %mip-obj-val)
    :double
  (problem :pointer))
;;/* retrieve objective value (MIP solution) */

(defcfun ("glp_mip_row_val" %mip-row-val)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve row value (MIP solution) */

(defcfun ("glp_mip_col_val" %mip-col-val)
    :double
  (problem :pointer)
  (index :int))
;;/* retrieve column value (MIP solution) */

#|
(defcfun ("glp_check_int" %check-int))      /* undocumented */
void lpx_check_int(LPX *lp, LPXKKT *kkt);
/* check integer feasibility conditions */
|#

(defcfun ("glp_reset_parms" %reset-parms)
    :void
  (problem :pointer))
;;/* reset control parameters to default values */

(defcfun ("glp_set_int_parm" %set-int-parm)
    :void
  (problem :pointer)
  (parameter control-parameter)
  (value :int))
;;/* set (change) integer control parameter */

(defcfun ("glp_get_int_parm" %get-int-parm)
    :int
  (problem :pointer)
  (parameter control-parameter))
;;/* query integer control parameter */

(defcfun ("glp_set_real_parm" %set-real-parm)
    :void
  (problem :pointer)
  (parameter control-parameter)
  (value :double))
;;/* set (change) real control parameter */

(defcfun ("glp_get_real_parm" %get-real-parm)
    :double
  (problem :pointer)
  (parameter control-parameter))
;;/* query real control parameter */
