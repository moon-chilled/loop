;;; compare symbols and keywords indiscriminantly
(define (symbol-equal symbol1 symbol2)
  (let ((f (lambda (x) (if (keyword? x) (keyword->symbol x) x))))
    (let ((symbol1 (f symbol1))
          (symbol2 (f symbol2)))
      (and (symbol? symbol1)
           (symbol? symbol2)
           (eq? symbol1 symbol2)))))

;;; This function generates code for destructuring a value according
;;; to a tree of variables.  D-VAR-SPEC is a tree of variable names
;;; (symbols).  FORM is a form that, at runtime, computes the value to
;;; be assigned to the root of D-VAR-SPEC.  This function returns a
;;; list of bindings to be used in a LET* form.  These bindings
;;; destructure the root value until the leaves of the tree are
;;; reached, generating intermediate temporary variables as necessary.
;;; The destructuring code calls the function LIST-CAR and LIST-CDR so
;;; that an error is signaled whenever the corresponding place in the
;;; value tree is not a CONS cell.
(define (destructure-variables d-var-spec form)
  (let ((bindings '()))
    (letrec ((traverse (lambda (d-var-spec form)
               (cond ((null? d-var-spec))
                     ((symbol? d-var-spec)
                      (push `(,d-var-spec ,form) bindings))
                     ((not (pair? d-var-spec))
                      (error 'expected-var-spec-but-found
                             :found d-var-spec))
                     (#t
                      (let ((temp (gensym)))
                        (push `(,temp ,form) bindings)
                        (traverse (car d-var-spec) `(,list-car ,temp))
                        (traverse (cdr d-var-spec) `(,list-cdr ,temp))))))))
      (traverse d-var-spec form)
      (reverse bindings))))

;;; Given a D-VAR-SPEC, compute a D-VAR-SPEC with the same structure
;;; as the one given as argument, except that the non-NIL leaves
;;; (i.e., the variables names) have been replaced by fresh symbols.
;;; Return two values: the new D-VAR-SPEC and a dictionary in the form
;;; of an association list that gives the correspondence between the
;;; original and the new variables.
(define (fresh-variables d-var-spec)
  (let* ((dictionary '()))
    (letrec ((traverse (lambda (d-var-spec)
               (cond ((null? d-var-spec) '())
                     ((symbol? d-var-spec)
                      (let ((temp (gensym)))
                        (push (cons d-var-spec temp) dictionary)
                        temp))
                     (#t
                      (cons (traverse (car d-var-spec))
                            (traverse (cdr d-var-spec))))))))
      (list (traverse d-var-spec)
            (reverse dictionary)))))

(define (generate-assignments d-var-spec form)
  (pidgin-destructuring-bind (temp-d-var-spec dictionary)
                             (fresh-variables d-var-spec)
    `(let* ,(destructure-variables temp-d-var-spec form)
       ,@(map (lambda (t) `(set! ,(car t) ,(cdr t))) dictionary))))

;;; Extract variables
(define (extract-variables d-var-spec d-type-spec)
  (let ((result '()))
    (letrec ((extract-aux (lambda (d-var-spec d-type-spec)
               (cond ((null? d-var-spec))
                     ((symbol? d-var-spec)
                      (push (list d-var-spec (or d-type-spec 't)) result))
                     ((type-specifier? d-type-spec)
                      (if (not (pair? d-var-spec))
                          (error 'expected-var-spec-but-found
                                 :found d-var-spec)
                          (begin (extract-aux (car d-var-spec) d-type-spec)
                                 (extract-aux (cdr d-var-spec) d-type-spec))))
                     ((not (pair? d-var-spec))
                      (error 'expected-var-spec-but-found
                             :found d-var-spec))
                     ((not (pair? d-type-spec))
                      (error 'expected-type-spec-but-found
                             :found d-type-spec))
                     (#t
                      (extract-aux (car d-var-spec) (if d-type-spec (car d-type-spec) #f))
                      (extract-aux (cdr d-var-spec) (if d-type-spec (cdr d-type-spec) #f)))))))
      (extract-aux d-var-spec d-type-spec)
      result)))
