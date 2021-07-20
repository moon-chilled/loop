;;;; Clause FINAL-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FINAL-CLAUSE.
;;;
;;; An FINAL clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    final-clause ::= finally compound-form+

(defclass final-clause (clause)
  (form)

  ;;; The final clause does not bind any variables.
  (bound-variables (clause) '())
  (accumulation-variables (clause) '())

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute epilogue.
  
  (epilogue-form (clause)
    (clause 'form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser final-clause-parser
  (consecutive (lambda (finally compound+)
                 (make-instance 'final-clause
                   :form compound+))
               (keyword-parser 'finally)
               compound+))

(add-clause-parser final-clause-parser)
