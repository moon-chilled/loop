;;;; Clause RETURN-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RETURN-CLAUSE.
;;;
;;; An RETURN clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    return-clause ::= return {form | it}

(defclass return-clause (unconditional-clause)
  ()
  (accumulation-variables (clause)
    '()))


(defclass return-it-clause (return-clause) ()
  (body-form (clause end-tag)
    (unless *it-var* (error "need an iteration variable in order to 'return it'"))
    `(,*loop-return-sym* ,*it-var*)))

(defclass return-form-clause (return-clause)
  (form)
  (body-form (clause end-tag)
    `(,*loop-return-sym* ,(clause 'form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser return-it-clause-parser
  (consecutive (lambda (return it)
                 (make-instance 'return-it-clause))
               (keyword-parser 'return)
               (keyword-parser 'it)))

(define-parser return-form-clause-parser
  (consecutive (lambda (return form)
                 (make-instance 'return-form-clause
                   :form form))
               (keyword-parser 'return)
               anything-parser))

(define-parser return-clause-parser
  (alternative return-it-clause-parser
               return-form-clause-parser))

(add-clause-parser return-clause-parser)
