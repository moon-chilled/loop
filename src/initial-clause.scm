;;;; Clause INITIAL-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INITIAL-CLAUSE.
;;;
;;; An INITIAL clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    initial-clause ::= initially compound-form+

(defclass initial-clause (clause)
  (form)

  ;;; The initial clause does not bind any variables.
  (bound-variables (clause) '())
  (accumulation-variables (clause) '())

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute prologue-form.
  
  (prologue-form (clause end-tag)
    (clause 'form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser initial-clause-parser
  (consecutive (lambda (initially compound+)
                 (make-instance 'initial-clause
                   :form compound+))
               (keyword-parser 'initially)
               compound+))

(add-clause-parser initial-clause-parser)
