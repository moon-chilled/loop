(define *loop-errors* (make-hash-table 8 eq?))

(define-macro (add-error-formatter name parameter-list . body)
  `(set! (*loop-errors* ',name)
         (lambda*
           (stream ,@(map (lambda (x) (if (pair? x) x `(,x (error "No argument supplied for parameter ~a" ',x))))
                          parameter-list))
           ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition reporters for parse errors.

(add-error-formatter expected-var-spec-but-end ()
  (format stream
          "Expected a variable specification, but reached the end of the loop body."))

(add-error-formatter expected-var-spec-but-found (found)
  (format stream
          "Expected a variable specification but found the following instead: ~s"
          found))

(add-error-formatter expected-simple-var-but-end ()
  (format stream
          "Expected a simple variable but reached the end of the loop body."))

(add-error-formatter expected-simple-var-but-found (found)
  (format stream
          "Expected a simple variable but found the following instead: ~s"
          found))

(add-error-formatter expected-type-spec-but-end ()
  (format stream
          "Expected a variable specification but reached the end of the loop body."))

(add-error-formatter expected-type-spec-but-found (found)
  (format stream
          "Expected a type specification but found the following instead: ~s"
          found))

(add-error-formatter expected-compound-form-but-end ()
  (format stream
          "Expected a compound form but reached the end of the loop body."))

(add-error-formatter expected-compound-form-but-found (found)
  (format stream
          "Expected a compound form but found the following instead: ~s"
          found))

(add-error-formatter expected-form-but-end ()
  (format stream
          "Expected a form but reached the end of the loop body."))

(add-error-formatter expected-symbol-but-end ()
  (format stream
          "Expected a symbol but reached the end of the loop body."))

(add-error-formatter expected-symbol-but-found (found)
  (format stream
          "Expected a symbol but found the following instead: ~s"
          found))

(add-error-formatter expected-keyword-but-found (found)
  (format stream
          "Expected a loop keyword, but found the following instead: ~s"
          found))

(add-error-formatter expected-for/as-subclause-but-end ()
  (format stream
          "Expected a loop keyword indicating a for/as subclause, but reached the end of the loop body."))

(add-error-formatter expected-symbol-but-found (found)
  (format stream
          "Expected a loop keyword indicating a for/as subclause, but found the following instead: ~s"
          found))

(add-error-formatter expected-each/the-but-end ()
  (format stream
          "Expected the loop keyword each/the, but reached the end of the loop body."))

(add-error-formatter expected-each/the-but-found (found)
  (format stream
          "Expected the loop keyword each/the, but found the following instead: ~s"
          found))

(add-error-formatter expected-in/of-but-end ()
  (format stream
          "Expected the loop keyword in/or, but reached the end of the loop body."))

(add-error-formatter expected-in/of-but-found (found)
  (format stream
          "Expected the loop keyword in/or, but found the following instead: ~s"
          found))

(add-error-formatter conflicting-stepping-directions ()
  (format stream "Conflicting stepping directions."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition reporters for syntax errors.

(add-error-formatter name-clause-not-first ()
  (format stream
          "A NAME loop clause was found, but it was not the first clause."))

(add-error-formatter multiple-name-clauses ()
  (format stream
          "Multiple NAME clauses where found."))

(add-error-formatter invalid-clause-order ()
  (format stream
          "Invalid clause order.  Variable clauses must precede main clauses."))

(add-error-formatter multiple-variable-occurrences (bound-variable)
  (format stream
          "Multiple occurrences of the following variable were found: ~s"
          bound-variable))
