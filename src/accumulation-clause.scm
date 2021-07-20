;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulation clauses

(defclass accumulation-clause (selectable-clause)
  ;;; The methods on ACCUMULATION-VARIABLES call the function INTO-VAR
  ;;; on the clause in order to obtain the first element of each
  ;;; accumulation variable descriptor.  For clauses that have
  ;;; INTO-MIXIN as a superclass, the variable is stored in a slot.
  ;;; This method defines the default method to be used for all other
  ;;; accumulation clauses.
  ((into-var #f))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Method on ACCUMULATION-VARIABLES, valid for all accumulation
  ;;; clauses.

  (accumulation-variables (clause)
    `((,(clause 'into-var)
       ,(clause 'accumulation-category)
       ,(clause 'type-spec)))))

;;; We define three different accumulation CATEGORIES, each identified
;;; by a symbol: LIST, COUNT/SUM, and MAX/MIN.  Accumulation clauses
;;; within a category are compatible in that they can be mixed, even
;;; when they accumulate into the same variable.  This generic
;;; function takes an accumulation clause and returns the category.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIST-ACCUMULATION-CLAUSE.
;;;
;;; This class is the superclass of the list accumulation clauses:
;;; COLLECT-CLAUSE, APPEND-CLAUSE, and NCONC-CLAUSE.
;;;

(defclass list-accumulation-clause (accumulation-clause)
  ((accumulation-category 'list)
   ;;; The methods on ACCUMULATION-VARIABLES call the function TYPE-SPEC
   ;;; on the clause in order to obtain the third element of each
   ;;; accumulation variable descriptor.  For the numeric accumulation
   ;;; clauses, the type is stored in a slot.  For the list accumulation
   ;;; clauses, we always want to return the type LIST.
   (type-spec 'list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NUMERIC-ACCUMULATION-CLAUSE.

(defclass numeric-accumulation-clause (accumulation-clause)
  ((type-spec 't)))

(defclass count/sum-accumulation-clause (numeric-accumulation-clause)
  ((accumulation-category 'count/sum)))

(defclass max/min-accumulation-clause (numeric-accumulation-clause)
  ((accumulation-category 'max/min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for INTO clause variants.

(defclass into-mixin ()
  ((into-var #f)))
