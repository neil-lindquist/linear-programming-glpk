;; Code heavily based off https://github.com/sellout/cl-glpk and https://github.com/phikal/cl-glpk-1
;; Used under the GPL 3.0 and BSD sans advertising licenses

(in-package :linear-programming-glpk)

(define-foreign-library libglpk
  (:windows (:or "glpk" "glpk_4_65"))
  (:unix (:or "libglpk.so.40"))
  (t (:default "libglpk")))

(use-foreign-library libglpk)


;; Interface based off glpk.h version 4.65

(defcenum optimization-direction-flag
  (:min 1)
  (:max 2))

(defcenum structural-variable-kind
  (:continuous 1)
  (:integer    2)
  (:binary     3))

(defcenum variable-type
  (:continuous 1)
  (:integer    2)
  (:binary     3))

;; type of auxiliary/structural variable
(defcenum variable-type
  (:free 1)         ;; /* free variable */
  (:lower-bound 2)  ;; /* variable with lower bound */
  (:upper-bound 3)  ;; /* variable with upper bound */
  (:double-bound 4) ;; /* double-bounded variable */
  (:fixed 5))       ;; /* fixed variable */

(defcenum variable-status
 (:basic 1)
 :non-basic-on-lower-bound
 :non-basic-on-upper-bound
 :non-basic-free
 :non-basic-fixed)

#| TODO Bit flags, not enum
(defcenum scaling-type
 (:geometric-mean #x1)
 (:equilibration #x10)
 (:power-of-two #x20)
 (:skip-if-well-scaled #x40)
 (:auto #x80))
|#

(defcenum solution-type
  (:basic 1)
  (:interior-point 2)
  (:integer 3))

(defcenum solution-status
  (:undefined 1)
  :feasible
  :infeasible
  :no-feasible-solution-exists
  :optimal
  :unbounded)

#| TODO struct
typedef struct
{...} glp_bfcp;
|#

(defcenum message-level
  (:off 0)
  (:error 1)
  (:on 2)
  (:all 3)
  (:debug 4))

(defcenum simplex-method
  (:primal 1)
  (:dual-primal 2)
  (:dual 3))

(defcenum simplex-pricing
  (:standard #x11)
  (:projected-steepest-edge #x22))

(defcenum ratio-test
  (:standard #x11)
  (:harris-2-pass #x22)
  (:flip-flop #x33))

(defcenum enablep
  (:on 1)
  (:off 0))

(defcstruct simplex-control-parameters
  (message-level message-level)
  (method simplex-method)
  (pricing simplex-pricing)
  (ratio-test ratio-test)
  (primal-feasibility-tol :double)
  (dual-feasibility-tol :double)
  (pivot-tol :double)
  (lower-obj-limit :double)
  (upper-obj-limit :double)
  (iteration-limit :int)
  (time-limit :int)
  (display-out-freq :int)
  (display-out-delay :int)
  (enable-presolver enablep)
  (exclude-fixed-vars :int)
  (shift-var-bounds :int)
  (a-or-n :int) ;; GLP_USE_AT = 1; GLP_USE_NT = 2
  (unused :double :count 33))

(defcenum ordering-algorithm
  (:none 0)
  (:quotient-min-degree 1)
  (:approx-min-degree 2)
  (:symmetric-approx-min-degree 3))

(defcstruct interior-point-control-parameters
  (message-level message-level)
  (ordering-algorithm ordering-algorithm)
  (unused :double :count 48))

(defcenum branching-technique
  (:first-fractional-variable 1)
  (:last-fractional-variable 2)
  (:most-fractional-variable 3)
  (:driebeck-tomlin 4)
  (:hybrid-pseudocost 5))

(defcenum backtracking-technique
  (:depth-first 1)
  (:breadth-first 2)
  (:best-local-bound 3)
  (:best-projection-heuristic 4))

(defcenum preprocessing-technique
  (:none 0)
  (:root-level 1)
  (:all-levels 2))

(defcstruct integer-control-parameters
  (message-level message-level)
  (branching-technique branching-technique)
  (backtracking-technique backtracking-technique)
  (integer-tolerance :double)
  (objective-tolerance :double)
  (time-limit :int)
  (display-out-freq :int)
  (display-out-delay :int)
  (cb-function :pointer)
  (cb-info :pointer)
  (preprocessing-technique preprocessing-technique)
  (relative-mip-gap-tol :double)
  (mir-cuts enablep)
  (gomory-cuts enablep)
  (cover-cuts enablep)
  (clique-cuts enablep)
  (presolve enablep)
  (binarize enablep)
  (feasibility-pump-heuristic enablep)
  (proxy-search-heuristic :int)
  (proxy-time-limit :int)
  (simple-rounding-heuristic :int)
  (use-sol :int)
  (save-sol :string)
  (alien :int)
  (flip :int)
  (unused :double :count 23))

#| TODO struct
typedef struct
{...} glp_attr;
|#

(defcenum reason-codes
  (:request-row-gen 1)
  (:better-int-sol 2)
  (:request-heuristic 3)
  (:request-cut-gen 4)
  (:request-branching 5)
  (:request-subproblem 6)
  (:request-preprocessing 7))

(defcenum branch-selection
  (:no-branch 0)
  (:down-branch 1)
  (:up-branch 2))

(defcenum solver-exit-code
  (:success #x0)
  (:invalid-basis #x1)
  (:singular-matrix #x2)
  (:ill-conditioned-matrix #x3)
  (:invalid-bounds #x4)
  (:solver-failed #x5)
  (:objective-lower-limit #x6)
  (:objective-upper-limit #x7)
  (:iteration-limit #x8)
  (:timie-limit #x9)
  (:no-primal-feasible-sol #xA)
  (:no-dual-feasible-sol #xB)
  (:no-root-lp-optimum #xC)
  (:search-terminated #xD)
  (:rel-min-gap-tol #xE)
  (:no-primal/duel-feasible-solution #xF)
  (:no-convergence #x10)
  (:instability #x11)
  (:invalid-data #x12)
  (:result-out-of-range #x13))

(defcenum kkt-condition
  (:primal-equalities 1)
  (:primal-bounds 2)
  (:dual-equalities 3)
  (:dual-bounds 4)
  (:complementary-slackness 5))

#| Skipping I/O code
/* MPS file format: */
#define GLP_MPS_DECK       1  /* fixed (ancient) */
#define GLP_MPS_FILE       2  /* free (modern) */

typedef struct
{     /* MPS format control parameters */
      int blank;
      /* character code to replace blanks in symbolic names */
      char *obj_name;
      /* objective row name */
      double tol_mps;
      /* zero tolerance for MPS data */
      double foo_bar[17];
      /* (reserved for use in the future) */}
glp_mpscp;

typedef struct
{     /* CPLEX LP format control parameters */
      double foo_bar[20];
      /* (reserved for use in the future) */}
glp_cpxcp;
|#

;; Line 306

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
  (rows :pointer)  ;; int array
  (cols :pointer)  ;; int array
  (vals :pointer)) ;; double array
;; /* load (replace) the whole constraint matrix */

(defcfun ("glp_check_dep" %check-dep)
  :int
  (m :int)
  (n :int)
  (ne :int)
  (rows :pointer)  ;; int array
  (cols :pointer)) ;; double array
;;/* check for duplicate elements in sparse matrix */

(defcfun ("glp_sort_matrix" %sort-matrix)
  :void
  (problem :pointer))
;; /* sort elements of the constraint matrix */

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

;; TODO unused
;;void glp_copy_prob(glp_prob *dest, glp_prob *prob, int names);
;;/* copy problem object content */

;; TODO unused
;;void glp_erase_prob(glp_prob *P);
;;/* erase problem object content */

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


;; TODO unused
;;void glp_set_rii(glp_prob *P, int i, double rii);
;;/* set (change) row scale factor */
;;void glp_set_sjj(glp_prob *P, int j, double sjj);
;;/* set (change) column scale factor */
;;double glp_get_rii(glp_prob *P, int i);
;;/* retrieve row scale factor */
;;double glp_get_sjj(glp_prob *P, int j);
;;/* retrieve column scale factor */

(defcfun ("glp_scale_prob" %scale-prob)
    :void
  (problem :pointer))
;;/* scale problem data */

(defcfun ("glp_unscale_prob" %unscale-prob)
    :void
  (problem :pointer))
;;/* unscale problem data */

(defcfun ("glp_set_row_stat" %set-row-stat)
    :void
  (problem :pointer)
  (index :int)
  (status variable-status))
;;/* set (change) row status */

(defcfun ("glp_set_col_stat" %set-col-stat)
    :void
  (problem :pointer)
  (index :int)
  (status variable-status))
;;/* set (change) column status */

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

(defcfun ("glp_simplex" %simplex)
    solver-exit-code
  (problem :pointer)
  (params (:pointer (:struct simplex-control-parameters))))
;;/* solve LP problem with the simplex method */

(defcfun ("glp_exact" %exact)
    solver-exit-code
  (problem :pointer)
  (params (:pointer (:struct simplex-control-parameters))))
;;/* easy-to-use driver to the exact simplex method */

(defcfun ("glp_init_smcp" %init-smcp)
  :void
  (paramters (:pointer (:struct simplex-control-parameters))))
;;/* initialize simplex method control parameters */

(defcfun ("glp_get_status" %get-status)
    solution-status
  (problem :pointer))
;;/* retrieve generic status of basic solution */

(defcfun ("glp_get_prim_stat" %get-prim-stat)
    solution-status
  (problem :pointer))
;;/* retrieve primal status of basic solution */

(defcfun ("glp_get_dual_stat" %get-dual-stat)
    solution-status
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

;;TODO unused
;;int glp_get_unbnd_ray(glp_prob *P);
;;/* determine variable causing unboundedness */
;;int glp_get_it_cnt(glp_prob *P);
;;/* get simplex solver iteration count */
;;void glp_set_it_cnt(glp_prob *P, int it_cnt);
;;/* set simplex solver iteration count */

;;Line  523

(defcfun ("glp_interior" %interior)
    solver-exit-code
  (problem :pointer)
  (parameters :pointer))
;;/* easy-to-use driver to the interior point method */

(defcfun ("glp_init_iptcp" %init-iptcp)
  :void
  (parameters :pointer))

(defcfun ("glp_ipt_status" %ipt-status)
    solution-status
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

(defcfun ("glp_intopt" %intopt)
    solver-exit-code
  (problem :pointer)
  (parameters (:pointer (:struct integer-control-parameters))))
;;/* easy-to-use driver to the branch-and-bound method */

(defcfun ("glp_init_iocp" %init-iocp)
  :void
  (parameters (:pointer (:struct integer-control-parameters))))
;;/* initialize integer optimizer control parameters */

(defcfun ("glp_mip_status" %mip-status)
    solution-status
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

;; TODO ommitting everything starting with line 577
