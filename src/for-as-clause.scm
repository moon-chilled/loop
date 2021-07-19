;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-CLAUSE.
;;;
;;; The HyperSpec says that a FOR-AS-CLAUSE has the following syntax:
;;;
;;;    for-as-clause ::= {for | as} for-as-subclause {and for-as-subclause}* 
;;;    for-as-subclause::= for-as-arithmetic | for-as-in-list | 
;;;                        for-as-on-list | for-as-equals-then | 
;;;                        for-as-across | for-as-hash | for-as-package 
;;;
;;; For the purpose of specialization, we need different names for the
;;; main clauses as well as for the subclauses, so we alter this
;;; grammar a bit and define it like this instead:
;;;
;;;    for-as-clause::= 
;;;      for-as-arithmetic-clause | for-as-in-list-clause | 
;;;      for-as-on-list-clause | for-as-equals-then-clause | 
;;;      for-as-across-clause | for-as-hash-clause | for-as-package-clause
;;;    
;;;    for-as-arithmetic-clause ::=
;;;      {for | as} for-as-arithmetic {and for-as-subclause}* 
;;;    
;;;    for-as-in-list-clause ::=
;;;      {for | as} for-as-in-list {and for-as-subclause}* 
;;;    
;;;    for-as-on-list-clause ::=
;;;      {for | as} for-as-on-list {and for-as-subclause}* 
;;;    
;;;    for-as-equals-then-clause ::=
;;;      {for | as} for-as-equals-then {and for-as-subclause}* 
;;;    
;;;    for-as-across-clause ::=
;;;      {for | as} for-as-across {and for-as-subclause}* 
;;;
;;;    for-as-hash-clause ::=
;;;      {for | as} for-as-hash {and for-as-subclause}* 
;;;
;;;    for-as-package-clause ::=
;;;      {for | as} for-as-package {and for-as-subclause}* 

(defclass for-as-clause (variable-clause subclauses-mixin) ()
  (bound-variables (clause)
    (apply append
            (map bound-variables (clause 'subclauses))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the bindings.
  
  (initial-bindings (clause)
    (apply append (map (lambda (x) (format #t "T ~a~%" (x 'class-name)) (initial-bindings x)) (clause 'subclauses))))
  
  (final-bindings (clause)
    (apply append (map final-bindings (clause 'subclauses))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the declarations.
  
  (declarations (clause)
    (apply append (map declarations (clause 'subclauses))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the prologue-form.
  
  (prologue-form (clause end-tag)
    `(begin ,@(map (lambda (subclause)
                     (prologue-form subclause end-tag))
                   (clause 'subclauses))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the termination-form.
  
  (termination-form (clause end-tag)
    `(begin ,@(map (lambda (subclause)
                        (termination-form subclause end-tag))
                      (clause 'subclauses))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the body-form.
  
  (body-form (clause end-tag)
    `(begin ,@(map (lambda (clause)
                        (body-form clause end-tag))
                      (clause 'subclauses))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Step a FOR-AS clause.
  
  (step-form (clause)
    `(begin ,@(map step-form (clause 'subclauses)))))

(defclass for-as-subclause (var-and-type-spec-mixin)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Manage a list of FOR-AS subclause parsers. 

(define *for-as-subclause-parsers* '())

(define (add-for-as-subclause-parser parser)
  (push parser *for-as-subclause-parsers*))

;;; A parser that tries every parser in *FOR-AS-SUBCLAUSE-PARSERS* until one
;;; succeeds.

(define (for-as-subclause-parser tokens)
  (let loop ((parsers *for-as-subclause-parsers*))
    (if (null? parsers)
      (list #f #f tokens)
      (pidgin-destructuring-bind (success result rest)
        ((car parsers) tokens)
        (if success
          (list #t result rest)
          (loop (cdr parsers)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a FOR-AS clause.

(define-parser for-as-clause-parser
  (consecutive (lambda (for subclause more-subclauses)
                 (make-instance 'for-as-clause
                   :subclauses (cons subclause more-subclauses)))
               (alternative (keyword-parser 'for)
                            (keyword-parser 'as))
               for-as-subclause-parser
               (repeat* list
                        (consecutive (lambda (and subclause)
                                       subclause)
                                     (keyword-parser 'and)
                                     for-as-subclause-parser))))

(add-clause-parser for-as-clause-parser)
