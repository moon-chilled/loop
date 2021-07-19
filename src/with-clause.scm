;;; Clause WITH-CLAUSE.
;;;
;;; A WITH-CLAUSE allows the creation of local variables.  It is
;;; executed once.
;;;
;;; The syntax of a with-clause is:
;;;
;;;    with-clause ::= WITH var1 [type-spec] [= form1]
;;;                    {AND var2 [type-spec] [= form2]}*
;;;
;;; where var1 and var2 are destructuring variable specifiers
;;; (d-var-spec) allowing multiple local variables to be created in a
;;; single with-clause by destructuring the value of the corresponding
;;; form.
;;;
;;; When there are several consecutive with-clause, the execution is
;;; done sequentially, so that variables created in one with-clause
;;; can be used in the forms of subsequent with-clauses.  If parallel
;;; creation of variables is wanted, then the with-clause can be
;;; followed by one or more and-clauses.
;;;
;;; The (destructuring) type specifier is optional.  If no type
;;; specifier is given, it is as if t was given.
;;;
;;; The initialization form is optional.  If there is a corresponding
;;; type specifier for a variable, but no initialization form, then
;;; the variable is initialized to a value that is appropriate for the
;;; type.  In particular, for the type t the value is nil, for the
;;; type number, the value is 0, and for the type float, the value is
;;; 0.0.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WITH-CLAUSE.
;;;

(defclass with-clause (variable-clause subclauses-mixin) ()
  (bound-variables (clause)
    (apply append (map bound-variables (clause 'subclauses))))
  (initial-bindings (clause)
    (apply append (map initial-bindings (clause 'subclauses)))))

(defclass with-subclause ()
  (var-spec
   type-spec
   ;; This slot contains a copy of the tree contained in the VAR-SPEC
   ;; slot except that the non-NIL leaves have been replaced by
   ;; GENSYMs.
   temp-vars
   ;; This slot contains a list of pairs.  Each pair is a CONS cell
   ;; where the CAR is a variable in VAR-SPEC and the CDR is the
   ;; corresponding variable in TEMP-VARS.
   dictionary
   ;;; The default form is NIL.
   (form '()))

  (bound-variables (subclause)
    (map car
         (extract-variables (subclause 'var-spec) #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the declarations.
  
  (declarations (clause)
    (apply append (map declarations (clause 'subclauses)))))


;(defmethod initialize-instance :after
;    ((clause with-subclause) &key &allow-other-keys)
;  (multiple-value-bind (temp-vars dictionary)
;      (fresh-variables (var-spec clause))
;    (reinitialize-instance clause
;                           :temp-vars temp-vars
;                           :dictionary dictionary)))

(defclass with-subclause-no-form (with-subclause) ()
  (initial-bindings (clause)
    `((,(clause 'form-var) ,(clause 'form))))

  (wrap-subclause (subclause inner-form)
    (let* ((vars-and-types
             (extract-variables (subclause 'var-spec) (subclause 'type-spec)))
           (vars-and-values
             (map (lambda (vt)
                    (list (car vt) (case (cadr vt)
                                     ((fixnum) 0)
                                     ((float) 0.0)
                                     (else #<unspecified>))))))) ;unspecified was nil in cl
      `(let ,vars-and-values
         ,inner-form))))

(defclass with-subclause-with-form (with-subclause)
  (form
   (form-var (gensym)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the subclause wrapper.
  
  (wrap-subclause (subclause inner-form)
    `(let* ,(destructure-variables (subclause 'var-spec) (subclause 'form-var))
       ,inner-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

;;; Parser for var [type-spec] = form
;;; We try this parser first.
(define-parser with-subclause-type-1-parser
  (consecutive (lambda (var-spec type-spec = form)
                 (make-with-subclause-with-form
                   :var-spec var-spec
                   :type-spec type-spec
                   :form form))
               ;; Accept anything for now.  Analyze later.
               anything-parser
               optional-type-spec-parser
               (keyword-parser '=)
               anything-parser))

;;; Parser for var [type-spec]
(define-parser with-subclause-type-2-parser
  (consecutive (lambda (var-spec type-spec)
                 (make-with-subclause-no-form
                   :var-spec var-spec
                   :type-spec type-spec))
               ;; Accept anything for now.  Analyze later.
               anything-parser
               optional-type-spec-parser))

;;; Parser for any type of with subclause without the leading keyword
(define-parser with-subclause-no-keyword-parser
  (alternative with-subclause-type-1-parser
               with-subclause-type-2-parser))

;;; Parser for the with subclause starting with the AND keyword.
(define-parser with-subclause-and-parser
  (consecutive (lambda (and subclause)
                 subclause)
               (keyword-parser 'and)
               with-subclause-no-keyword-parser))

;;; Parser for a with clause
(define-parser with-clause-parser
  (consecutive (lambda (with first rest)
                 (make-with-clause
                   :subclauses (cons first rest)))
               (keyword-parser 'with)
               with-subclause-no-keyword-parser
               (repeat* list
                        with-subclause-and-parser)))

(add-clause-parser with-clause-parser)
