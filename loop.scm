; automatically generated; do not edit

(define loop (let ()
(define transform-tagbody
  (let ()
    ;; from guile-user I think
    ;; (block LABEL FORMS...)
    ;;
    ;; Execute FORMS.  Within FORMS, a lexical binding named LABEL is
    ;; visible that contains an escape function for the block.  Calling
    ;; the function in LABEL with a single argument will immediatly stop
    ;; the execution of FORMS and return the argument as the value of the
    ;; block.  If the function in LABEL is not invoked, the value of the
    ;; block is the value of the last form in FORMS.

    (define-macro (block label . forms)
                  `(let ((body (lambda (,label) ,@forms))
                         (tag (gensym "return-")))
                     (catch tag
                            (lambda () (body (lambda (val) (throw tag val))))
                            (lambda (tag val) val))))

    ;; (with-return FORMS...)
    ;;
    ;; Equivalent to (block return FORMS...)

    (define-macro (with-return . forms)
                  `(,block return ,@forms))

    ;; (tagbody TAGS-AND-FORMS...)
    ;;
    ;; TAGS-AND-FORMS is a list of either tags or forms.  A TAG is a
    ;; symbol while a FORM is everything else.  Normally, the FORMS are
    ;; executed sequentially.  However, control can be transferred to the
    ;; forms following a TAG by invoking the tag as a function.  That is,
    ;; within the FORMS, there is a lexical binding for each TAG with the
    ;; symbol that is the tag as its name.  The bindings carry functions
    ;; that will execute the FORMS following the respective TAG.
    ;;
    ;; The value of a tagbody is always `#f'.

    (define (transform-tagbody forms)
      (let ((start-tag (gensym "start-"))
            (block-tag (gensym "block-")))
        (let loop ((cur-tag start-tag)
                   (cur-code ())
                   (tags-and-code ())
                   (forms forms))
          (cond
            ((null? forms)
             `(,block ,block-tag
                     (letrec ,(reverse! (cons (list cur-tag `(lambda () ,@(reverse! (cons `(,block-tag #f) cur-code)))) tags-and-code))
                       (,start-tag))))
            ((symbol? (car forms))
             (loop (car forms)
                   '()
                   (cons (list cur-tag `(lambda () ,@(reverse! (cons `(,(car forms)) cur-code)))) tags-and-code)
                   (cdr forms)))
            (else
              (loop cur-tag
                    (cons (car forms) cur-code)
                    tags-and-code
                    (cdr forms)))))))
    transform-tagbody))
;(define-macro (transform-tagbody . forms) (transform-tagbody forms))
(define-macro (push v s)
  (unless (symbol? s) (error))
  `(begin (set! ,s (cons ,v ,s)) ,s))

(define (list* . p)
  (if (null? (cdr p))
    (car p)
    (cons (car p) (apply list* (cdr p)))))

; a la j: x f&g y ←→ (f x) g (f y)
; hence, ((& f g) x y z...) ←→ (f (g x) (g y) (g z)...)
(define (compose f g) (lambda args (apply f (map g args))))
; and here: x (f g) y ←→ x f g y
; ((hook f g) x y) ←→ (f x (g y))
(define (hook f g) (lambda (x y) (f x (g y))))
; in particular, cl (higher-order-fn :pred f :key g)
; can be expressed here using simply (higher-order fn :pred (hook f g))
(define (bind f . rest) (lambda args (apply f (append rest args))))
(define (rbind f . rest) (lambda args (apply f (append args rest))))

(define (filter f xs)
  (cond
    ((null? xs) '())
    ((f (car xs)) (cons (car xs) (filter f (cdr xs))))
    (#t (filter f (cdr xs)))))
(define (any f l) (not (null? (filter f l))))

(define (constantly val)
  (lambda - val))

(define (identity x) x)

(define (remove-duplicates l pred)
  (let ((ret '()))
    (let loop ((l l))
      (unless (null? l)
        (unless (any (lambda (x) (pred x (car l))) ret)
          (push (car l) ret))
        (loop (cdr l))))
    (reverse ret)))

(define (remove-duplicates-from-end l pred)
  (let ((ret '()))
    (let loop ((l l))
      (unless (null? l)
        (loop (cdr l))
        (unless (any (lambda (x) (pred x (car l))) ret)
          (push (car l) ret))))
    ret))

(define (every f l)
  (if (null? l)
    #t
    (and (f (car l))
         (every f (cdr l)))))

(define (position-if pred l)
  (let loop ((l l)
             (i 0))
    (cond
      ((null? l) #f)
      ((pred (car l)) i)
      (#t (loop (cdr l) (+ 1 i))))))

(define (position-if-from-end pred l)
  (let loop ((l l)
             (i 0))
    (cond ((null? l) #f)
          ((pred (car l)) (or (loop (cdr l) (+ 1 i)) i))
          (#t (loop (cdr l) (+ 1 i))))))

(define (for-each-on f xs)
  (if (null? xs)
    '()
    (begin
      (f xs)
      (for-each-on f (cdr xs)))))

(define (count x xs pred)
  (let loop ((xs xs)
             (acc 0))
    (if (null? xs) acc
      (loop (cdr xs) (+ acc (if (pred x (car xs)) 1 0))))))

(define (intersection x y pred)
  (filter (rbind member y pred) x))

; to make up for multiple-value-bind
(define-macro (pidgin-destructuring-bind spec var . body)
  (let ((v (gensym)))
    (letrec ((expand (lambda (spec)
                       (if (null? spec)
                         `((unless (null? ,v) (error "too many values specified for destructuring")))
                         (list* `(set! ,(car spec) (car ,v))
                                `(set! ,v (cdr ,v))
                                (expand (cdr spec)))))))
      `(let ((,v ,var)
             ,@(map (lambda (s) `(,s #f)) spec))
         ,@(expand spec)
         ,@body))))

; simple stub generics implementation (single dispatch only)
; (defgeneric m (x y z) body*)  will define a function 'm' that expects 'x' to
; be a let containing a bound lambda 'm'; y and z will be passed to that
; lambda.  If 'm' is not bound, body will be evaluated instead.  If body is
; nil, an error will be signaled

(define-macro (defgeneric name pspec . body)
  `(define (,name ,@pspec)
     (if (eq? ,name (,(car pspec) ',name))
       ,(if (null? body) `(error "No method ~a bound in class ~a" ',name (,(car pspec) 'class-name)) `(begin ,@body))
       ((,(car pspec) ',name) ,@pspec))))

(define *classes* (make-hash-table 8 eq?))

(define (type-specifier? x) (or (symbol? x) (eq? x #f)))

(define-macro (defclass name super slots . methods)
  (when (*classes* name) (error "Class already defined: ~a" name))
  (let* ((super (map (lambda (s) (let ((r (*classes* s))) (unless r (error "No superclass ~a" s)) r)) super))
         (auxiliary-slots   (apply append (map (lambda (c) (c 'all-slots)) super)))
         (auxiliary-methods (apply append (map (lambda (c) (c 'all-methods)) super)))
         (methods (map (lambda (m) `(,(car m) (lambda* ,(cadr m) ,@(cddr m)))) methods))
         (slots (map (lambda (s) (if (pair? s) s `(,s (error ,(format #f "No initializer supplied for slot ~a" s))))) slots))
         (accessor-methods '())
         (all-slots (remove-duplicates-from-end `(,@auxiliary-slots ,@slots) (compose eq? car)))
         (all-methods (remove-duplicates-from-end `(,@auxiliary-methods ,@accessor-methods ,@methods) (compose eq? car)))
         (classes (cons name (remove-duplicates (apply append (map (lambda (x) (x 'classes)) super)) eq?))))
    `(set! (*classes* ',name)
           (inlet 'all-slots ',all-slots
                  'all-methods ',all-methods
                  'class-name ',name
                  'classes ',classes
                  'make (lambda* ,all-slots
                          (let ((class-name ',name)
                                ,@all-methods)
                            (curlet)))))))

(define (make-instance what . p)
  (apply ((*classes* what) 'make) p))

(define (type? var type) (member type ((*classes* (var 'class-name)) 'classes)))
;;; The purpose of this generic function is to generate a list of all
;;; bound variables in a clause.  The same variable occurs as many
;;; times in the list as the number of times it is bound in the
;;; clause.
(defgeneric bound-variables (clause))

;;; The purpose of this generic function is to generate a list of all
;;; the accumulation variables in a clause.  Each element of the list
;;; is itself a list of three elements.  The first element is the name
;;; of a variable used in an INTO clause, or NIL if the clause has no
;;; INTO.  The second element determines the kind of accumulation, and
;;; can be one of the symbols LIST, COUNT/SUM, or MAX/MIN.  The third
;;; element is a type specifier which can be T.
; NB. the scheme port uses #f instead of nil.
(defgeneric accumulation-variables (clause))

;;; The purpose of this generic function is to extract a list of
;;; declaration specifiers from the clause.  Notice that it is a list
;;; of declaration specifiers, not a list of declarations.  In other
;;; words, the symbol DECLARE is omitted.
(defgeneric declarations (clause)
  '())

(defgeneric initial-bindings (clause)
  '())

(defgeneric final-bindings (clause)
  '())

(defgeneric bindings (clause)
  (append (initial-bindings clause) (final-bindings clause)))

;;; This generic function returns a form for CLAUSE that should go in
;;; the LOOP prologue.  The INITIALLY clause is an obvious candidate
;;; for such code.  But the stepping clauses also have code that goes
;;; in the prologue, namely an initial termination test to determine
;;; whether any iterations at all should be executed.  END-TAG is the
;;; tag to GO to when the initial termination test says that no
;;; iterations should be executed.
(defgeneric prologue-form (clause end-tag)
  '())

;;; This generic function returns a form for CLAUSE that should go
;;; between the body code and the stepping forms in the body of the
;;; expanded code.  Some of the FOR-AS clauses and also the REPEAT
;;; clause generate code here.  END-TAG is the tag to GO to when
;;; iteration should terminate.
(defgeneric termination-form (clause end-tag)
  '())

;;; This generic function returns a form for CLAUSE that should go in
;;; the main the body code, before the termination test and the
;;; stepping forms, in the body of the expanded code.  The DO clause
;;; and the accumulation clauses are obvious candidates for such code.
;;;
;;; FIXME: Currently, END-TAG is used only in the WHILE clause as a
;;; termination test.  Investigate whether the WHILE clause should use
;;; TERMINATION-TEST instead, so that we can eliminate this parameter.
(defgeneric body-form (clause end-tag)
  '())

;;; This generic function returns a form for CLAUSE that should go
;;; after the main body code and the termination tests in the body of
;;; the expanded code.  The FOR-AS clauses and also the REPEAT clause
;;; generate code here.
(defgeneric step-form (clause)
  '())

;;; This generic function returns a form for CLAUSE that should go in
;;; the LOOP epilogue.  Of the clause types defined by the Common Lisp
;;; standard, only the method specialized to the FINALLY clause
;;; returns a value other than NIL.
(defgeneric epilogue-form (clause)
  '())

;;; Once the LOOP prologue, the LOOP body, and the LOOP epilogue have
;;; all been constructed, a bunch of successive WRAPPERS are applied
;;; so as to obtain the final expansion.  Each clause type defines how
;;; it needs to be wrapped.  Some clauses only require the
;;; establishment of variable bindings in the wrapper.  Other clauses
;;; might need to be wrapped in some iterator form.  The generic
;;; function WRAP-CLAUSE defines how each clause type is wrapped.
;;; The default method is applicable only if the clause type does not
;;; admit any subclauses.  For this type of clause, the default
;;; implemented here is to wrap the clause in all the bindings, i.e.,
;;; both the initial and the final bindings of both exist.
(defgeneric wrap-clause (clause inner-form)
  `(let* ,(bindings clause)
     ,inner-form))

;;; If a clause can have subclauses, then each subclause may need to
;;; be wrapped separately.  The generic function WRAP-SUBCLAUSE
;;; determines how this is done.
;;; By default, the wrapper for each subclause contains only the final
;;; bindings, leaving the initial bindings to a single binding form of
;;; the entire clause.
(defgeneric wrap-subclause (subclause inner-form)
  `(let ,(final-bindings subclause)
     ,inner-form))

;;; This variable is bound by the code generator for
;;; CONDITIONAL-CLAUSE before calling the code generators for the
;;; clauses in its THEN and ELSE branches.
(define *it-var* #f)

(define *accumulation-variable* #f)

(define *list-tail-accumulation-variable* #f)

(define *tail-variables* #f)

(define *loop-name* #f)

(define *loop-return-sym* #f)


(define *indent-level* 0)
(define *parse-trace?* #f)
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
    (if (null? dictionary)
      ()
      `(let* ,(destructure-variables temp-d-var-spec form)
         ,@(map (lambda (t) `(set! ,(car t) ,(cdr t))) dictionary)))))

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
;;; A parser is a function that takes a list of tokens to parse, and
;;; that returns three values:
;;;
;;;   * A generalized Boolean indicating whether the parse succeeded.
;;;
;;;   * The result of the parse.  If the parse does not succeed, then
;;;     this value is unspecified.
;;;
;;;   * A list of the tokens that remain after the parse.  If the
;;;     parse does not succeed, then this list contains the original
;;;     list of tokens passed as an argument.

;;; Functions that take one or more parsers as arguments can take
;;; either a function or the name of a function.

(define (parse-trace-output format-control . arguments)
  (when *parse-trace?*
    (format #t (make-string (* 2 *indent-level*) #\space))
    (apply format #t format-control arguments)))

(define (trace-parser name parser tokens)
  (let-temporarily ((*indent-level* (+ 1 *indent-level*)))
    (parse-trace-output "trying ~s on ~s~%" name tokens)
    (pidgin-destructuring-bind (successp result rest)
        (parser tokens)
      (parse-trace-output "~asuccess~%" (if successp "" "no "))
      (list successp result rest))))

(define-macro (define-parser name . body)
  `(define (,name tokens) (trace-parser ',name (begin ,@body) tokens)))

;;; Take a function designator (called the TRANSFORMER) and a
;;; predicate P and return a parser Q that invokes the predicate on
;;; the first token.  If P returns true then Q succeeds and returns
;;; the result of invoking TRANSFORMER on the token together with the
;;; remaining tokens (all tokens except the first one).  If P returns
;;; false, then Q fails.  If there are no tokens, then Q also fails.
(define (singleton transformer predicate)
  (lambda (tokens)
    (if (and (not (null? tokens))
             (predicate (car tokens)))
        (list #t (transformer (car tokens)) (cdr tokens))
        (list #f #f tokens))))

;;; Take a list of parsers P1, P2, ..., Pn and return a parser Q that
;;; invokes Pi in order until one of them succeeds.  If some Pi
;;; succeeds. then Q also succeeds with the same result as Pi.  If
;;; every Pi fails, then Q also fails.
(define (alternative . parsers)
  (lambda (tokens)
    (let loop ((parsers parsers))
      (if (null? parsers)
        (list #f #f tokens)
        (pidgin-destructuring-bind (success result rest)
          ((car parsers) tokens)
          (if success
            (list #t result rest)
            (loop (cdr parsers))))))))

;;; Take a function designator (called the COMBINER) and a list of
;;; parsers P1, P2, ..., Pn and return a parser Q that invokes every
;;; Pi in order.  If any Pi fails, then Q fails as well.  If every Pi
;;; succeeds, then Q also succeeds and returns the result of calling
;;; APPLY on COMBINER and the list of results of the invocation of
;;; each Pi.
(define (consecutive combiner . parsers)
  (lambda (tokens)
    (let loop ((remaining-tokens tokens)
               (remaining-parsers parsers)
               (results '()))
      (if (null? remaining-parsers)
        (list #t (apply combiner (reverse results)) remaining-tokens)
        (pidgin-destructuring-bind (success result rest)
          ((car remaining-parsers) remaining-tokens)
          (if success
            (loop rest (cdr remaining-parsers) (cons result results))
            (list #f #f tokens)))))))

;;; Take a function designator (called the COMBINER) and a parser P
;;; and return a parser Q that invokes P repeatedly until it fails,
;;; each time with the tokens remaining from the previous invocation.
;;; The result of the invocation of Q is the result of calling APPLY
;;; on COMBINER and the list of the results of each invocation of P.
;;; Q always succeeds.  If the first invocation of P fails, then Q
;;; succeeds returning the result of calling APPLY on COMBINER and the
;;; empty list of results, and the original list of tokens as usual.
(define (repeat* combiner parser)
  (lambda (tokens)
    (let loop ((remaining-tokens tokens)
               (results '()))
      (pidgin-destructuring-bind (success result rest)
          (parser remaining-tokens)
        (if success
          (loop rest (cons result results))
          (list #t (apply combiner (reverse results)) remaining-tokens))))))

;;; Take a function designator (called the COMBINER) and a parser P
;;; and return a parser Q that invokes P repeatedly until it fails,
;;; each time with the tokens remaining from the previous invocation.
;;; The result of the invocation of Q is the result of calling APPLY
;;; on COMBINER and the list of the results of each invocation of P.
;;; Q succeeds if and only if at least one invocation of P succeeds.
(define (repeat+ combiner parser)
  (lambda (tokens)
    (pidgin-destructuring-bind (success result rest)
        (parser tokens)
        (if (not success)
          (list #f #f tokens)
          (let loop ((remaining-tokens rest)
                     (results (list result)))
            (pidgin-destructuring-bind (success result rest)
                (parser remaining-tokens)
              (if success
                (loop rest (cons result results))
                (list #t (apply combiner (reverse results)) remaining-tokens))))))))

;;; Take a default value and a parser P and return a parser Q that
;;; always succeeds.  Q invokes P once.  If P succeeds, then Q
;;; succeeds with the same result as P and with the same remaining
;;; tokens.  If P fails, then Q succeeds, returning the default value
;;; and the original list of tokens.
(define (optional default parser)
  (lambda (tokens)
    (pidgin-destructuring-bind (success result rest)
        (parser tokens)
      (if success
          (list #t result rest)
          (list #t default tokens)))))

;;;  LocalWords:  parsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given a symbol S (no matter what package), return a singleton
;;; parser Q that recognizes symbols with the same name as S.  If Q
;;; succeeds, it returns S.

(define (keyword-parser symbol)
  (singleton (constantly symbol)
             (lambda (token) (symbol-equal symbol token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for anything, i.e. a parser that succeeds whenever the list
;;; of tokens is not empty.  It returns the first token as a result of
;;; the parse, and the list of tokens with the first one removed as
;;; the list of remaining tokens.

(define-parser anything-parser
  (singleton identity (constantly #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A parser that recognizes one of the LOOP keywords EACH and THE.
;;; It is used to parse FOR-AS-HASH and FOR-AS-PACKAGE subclauses.

(define-parser each-the-parser
  (alternative (keyword-parser 'each)
               (keyword-parser 'the)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A parser that recognizes one of the LOOP keywords IN and OF.
;;; It is used to parse FOR-AS-HASH and FOR-AS-PACKAGE subclauses.

(define-parser in-of-parser
  (alternative (keyword-parser 'in)
               (keyword-parser 'of)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A parser that recognizes one of the LOOP keyword BEING.
;;; It is used to parse FOR-AS-HASH and FOR-AS-PACKAGE subclauses.

(define-parser being-parser
  (keyword-parser 'being))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for COMPOUND-FORM+, i.e. a non-empty sequence of compound
;;; forms.

(define-parser compound+
  (repeat+ (lambda forms
             (cons 'begin forms))
           (singleton identity pair?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This parser succeeds whenever the list of tokens is either empty
;;; or starts with a form that is not a loop keyword that can start a
;;; clause.  When it succeeds, it returns NIL as the result and the
;;; original list of tokens.

(define *clause-keywords*
  '(initially finally
    with
    do return
    collect collecting
    append appending
    nconc nconcing
    count counting
    sum summing
    maximize maximizing
    minimize minimizing
    if when unless
    while until repeat always never thereis
    for as))

(define (non-clause-keyword tokens)
  (if (or (null tokens)
          (member (car tokens) *clause-keywords*
                  symbol-equal))
      (list #t #f tokens)
      (list #f #f tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Manage a list of clause parsers.

(define *clause-parsers* '())

(define (add-clause-parser parser)
  (push parser *clause-parsers*))

;;; A parser that tries every parser in *CLAUSE-PARSERS* until one
;;; succeeds.

(define (parse-clause tokens)
  (let loop ((parsers *clause-parsers*))
    (if (null? parsers)
      (list #f #f tokens)
      (pidgin-destructuring-bind (success result rest)
        ((car parsers) tokens)
        (if success
          (list #t result rest)
          (loop (cdr parsers)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOOP-BODY.
;;;
;;; An instance of this class is the result of parsing the clauses.

;(defclass loop-body ()
;  ((%clauses :initform '() :initarg :clauses :accessor clauses)
;   (%accumulation-variable :initform nil :accessor accumulation-variable)
;   (%accumulation-list-tail :initform nil :accessor accumulation-list-tail)
;   (%accumulation-type :initform nil :accessor accumulation-type)))

;;; Create a list of clauses from the body of the LOOP form.
(define (parse-loop-body body)
  (let loop ((remaining-body body)
             (clauses '()))
    (if (null? remaining-body)
      (reverse clauses)
      (pidgin-destructuring-bind (success clause rest)
        (parse-clause remaining-body)
        (if success
          (loop rest (cons clause clauses))
          ;; FIXME: this is not the right error to signal.
          (error 'expected-keyword-but-found
                 :found (car rest)))))))
;;;; The terminology used here is that of the BNF grammar in the
;;;; dictionary description of the loop macro in the HyperSpec.  It is
;;;; not the same as the terminology used in the section 6.1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Common classes.

;;; The base class of all clauses.
(defclass clause () ())

;;; Mixin for clauses that accept `AND'.
(defclass subclauses-mixin ()
  (subclauses)
  ;;; Method on WRAP-CLAUSE specialized to clause types that admit
  ;;; subclauses.  This method overrides the default method above.  It
  ;;; wraps each subclause individually, and then wraps the result in
  ;;; the initial bindings for the entire clause.
  (wrap-clause (clause inner-form)
    (let ((result inner-form))
      (map (lambda (subclause)
              (set! result (wrap-subclause subclause result)))
            (reverse (clause 'subclauses)))
      `(let ,(initial-bindings clause)
         ,result))))

;;; Mixin for clauses and subclauses that take
;;; a VAR-SPEC and a TYPE-SPEC.
(defclass var-and-type-spec-mixin ()
  (var-spec type-spec))

;;; Mixin for clauses that take a list of compound forms.
(defclass compound-forms-mixin ()
  (forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that make the loop return a value.

(defclass loop-return-clause-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that has an implicit IT argument.

(defclass it-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that has an explicit form argument.

(defclass form-mixin ()
  (form))
(define (tail-variable head-variable)
  (let ((result (*tail-variables* head-variable)))
    (unless result
      (set! result (gensym))
      (set! (*tail-variables* head-variable) result))
    result))

(define (accumulation-bindings clauses)
  (let* ((descriptors
           (apply append
                  (map accumulation-variables clauses)))
         (equal-fun (lambda (d1 d2)
                      (and (eq? (car d1) (car d2))
                           (eq? (cadr d1) (cadr d2)))))
         (unique (remove-duplicates descriptors equal-fun)))
    (let loop ((unique unique))
      (if (null? unique)
        '()
        (let ((name (caar unique))
              (category (cadar unique))
              (type (caddar unique)))
          (let ((initial-value (cond ((eq? category 'count/sum) (car (arithmetic-value-and-type type))) ;(coerce 0 type)
                                     ((eq? category 'always/never) #t)
                                     ((eq? category 'max) -inf.0)
                                     ((eq? category 'min) +inf.0)
                                     (#t ''()))))
            (append
              (if (not name)
                `((,*accumulation-variable* ,initial-value))
                `((,name ,initial-value)))
              (if (eq? category 'list)
                (if (not name)
                  `((,*list-tail-accumulation-variable* '()))
                  `((,(tail-variable name) '())))
                '())
              (loop (cdr unique)))))))))

(define *clause* #f)
(define (prologue-body-epilogue clauses end-tag)
  (let ((start-tag (gensym)))
    (transform-tagbody
        `((begin ,@(map (lambda (clause)
                          (prologue-form clause end-tag))
                        clauses))
          ,start-tag
          (begin ,@(map (lambda (clause)
                          (body-form clause end-tag))
                        clauses))
          (begin ,@(map (lambda (clause)
                          (termination-form clause end-tag))
                        clauses))
          (begin ,@(map step-form clauses))
          (,start-tag)
          ,end-tag
          (begin ,@(map epilogue-form clauses)
                 (,*loop-return-sym*
                   ,*accumulation-variable*))))))

;;; Process all clauses by first computing the prologue, the body, and
;;; the epilogue, and then applying the clause-specific wrapper for
;;; each clause to the result.
(define (do-clauses all-clauses end-tag)
  (let ((result (prologue-body-epilogue all-clauses end-tag)))
    (map (lambda (clause)
           (set! result (wrap-clause clause result)))
         (reverse all-clauses))
    result))

(define (expand-clauses all-clauses end-tag)
  (let ((acc (accumulation-bindings all-clauses)))
    `(let (,@(if (member *accumulation-variable* (map car acc))
                 '()
                 `((,*accumulation-variable* '()))) ;*accumulation-variable* was nil originally; is '() right?
           ,@acc)
       ,(do-clauses all-clauses end-tag))))

(define (expand-body loop-body)
  (if (every pair? loop-body)
      (let ((tag (gensym)))
        `(call-with-exit
           (letrec ((,tag (lambda (return)
                            ,@loop-body
                            (,tag return))))
             ,tag)))
      (let ((clauses (parse-loop-body loop-body))
            (end-tag (gensym)))
        (analyze-clauses clauses)
        (let-temporarily ((*loop-name* (if (type? (car clauses) 'name-clause)
                                         ((car clauses) 'name)
                                         #f))
                          (*loop-return-sym* (gensym))
                          (*accumulation-variable* (gensym))
                          (*list-tail-accumulation-variable* (gensym))
                          (*tail-variables* (make-hash-table 8 eq?)))
          ; todo incorporate *loop-name* to allow named return
          `(call-with-exit
             (lambda (return)
               (call-with-exit
                 (lambda (,*loop-return-sym*)
                   (let ((loop-finish (macro () `(,',end-tag))))
                     ,(expand-clauses clauses end-tag))))))))))
;;; In the dictionary entry for LOOP, the HyperSpec says:
;;;
;;;   main-clause ::= unconditional |
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test |
;;;                   initial-final
;;;
;;; Here, we exclude initial-final.  The reason for that is that
;;; initial-final is also one of the possibilities for a
;;; variable-clause, and the reason for this "multiple inheritance" is
;;; so that the LOOP macro syntax can be defined to have the syntax:
;;;
;;;   loop [name-clause] {variable-clause}* {main-clause}*
;;;
;;; which then allows for INITIALLY and FINALLY clauses to occur
;;; anywhere after the name-clause.
;;;
;;; What we do here is to treat INITIALLY and FINALLY specially, so
;;; that they are neither main clauses nor variable clauses.
;;; Therefore, here, we have:
;;;
;;;   main-clause ::= unconditional |
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test
;;;
;;; Furthermore, the HyperSpec defines selectable-clause like this:
;;;
;;;   selectable-clause ::= unconditional | accumulation | conditional
;;;
;;; so we can say:
;;;
;;;    main-clause ::= selectable-clause | termination-test

(defclass main-clause (clause) ())
;;; In the dictionary entry for LOOP, the HyperSpec says:
;;;
;;;   variable-clause ::= with-clause | initial-final | for-as-clause
;;;
;;; Here, we exclude initial-final.  The reason for that is that
;;; initial-final is also one of the possibilities for a
;;; main-clause, and the reason for this "multiple inheritance" is
;;; so that the LOOP macro syntax can be defined to have the syntax:
;;;
;;;   loop [name-clause] {variable-clause}* {main-clause}*
;;;
;;; which then allows for INITIALLY and FINALLY clauses to occur
;;; anywhere after the name-clause.
;;;
;;; What we do here is to treat INITIALLY and FINALLY specially, so
;;; that they are neither main clauses nor variable clauses.
;;; Therefore, here, we have:
;;;
;;;   variable-clause ::= with-clause | for-as-clause

(defclass variable-clause (clause) ()
  ;;; No variable clause defines any accumulation variables
  (accumulation-variables (clause)
    '()))
;;; Recall that in the dictionary entry for LOOP, the HyperSpec says:
;;;
;;;   main-clause ::= unconditional |
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test |
;;;                   initial-final
;;;
;;; Though here, we exclude initial-final so that we have:
;;;
;;;   main-clause ::= unconditional |
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test
;;;
;;; Furthermore, the HyperSpec defines selectable-clause like this:
;;;
;;;   selectable-clause ::= unconditional | accumulation | conditional
;;;
;;; so we can say:
;;;
;;;    main-clause ::= selectable-clause | termination-test

(defclass selectable-clause (main-clause) ()
  (bound-variables (clause)
    '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser selectable-clause-parser
  (alternative do-clause-parser
               return-clause-parser
               collect-clause-parser
               append-clause-parser
               nconc-clause-parser
               count-clause-parser
               sum-clause-parser
               maximize-clause-parser
               minimize-clause-parser
               conditional-clause-parser))

(define-parser and-selectable-clause-parser
  (consecutive (lambda (and selectable-clause)
                 selectable-clause)
               (keyword-parser 'and)
               selectable-clause-parser))
(defclass unconditional-clause (selectable-clause) ())
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for INTO clause variants.

(defclass into-mixin ()
  ((into-var #f)))
;;; We define a class that is the root class of all termination-test
;;; clauses.  Recall that a termination-test clause is a main clause,
;;; and that the HyperSpec defines TERMINATION-TEST as follows:
;;;
;;;   termination-test ::= while form |
;;;                        until form |
;;;                        repeat form |
;;;                        always form |
;;;                        never form |
;;;                        thereis form

(defclass termination-test-clause (main-clause) ()
  ;;; The termination-test clauses do not bind any variables.
  (bound-variables (clause) '())
  (accumulation-variables (clause) '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for d-var-spec.

;;; A d-var-spec is a is a destructuring variable specifier:
;;;
;;;    d-var-spec ::= simple-var | nil | (d-var-spec . d-var-spec)
;;;
;;; where simple-var is a symbol (a name of a variable).
;;;

;;; Return true if and only if the argument is a valid d-var-spec, in
;;; other words if it is a tree of CONS cells where the leaves are
;;; symbols.
(define (d-var-spec-p object)
  (or (symbol? object)
      (and (pair? object)
           (d-var-spec-p (car object))
           (d-var-spec-p (cdr object)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser simple-type-spec-parser
  (lambda (tokens)
    (if (and (not (null? tokens))
             (member (car tokens) '(fixnum float t nil)))
        (list #t
                (car tokens)
                (cdr tokens))
        (list #f #f tokens))))

(define-parser destructured-type-spec-parser
  (consecutive (lambda (of-type tree)
                 tree)
               (keyword-parser 'of-type)
               anything-parser))

(define-parser type-spec-parser
  (alternative simple-type-spec-parser destructured-type-spec-parser))

(define-parser optional-type-spec-parser
  (optional #f type-spec-parser))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause NAME-CLAUSE.
;;;
;;; A NAME-CLAUSE is a clause that gives a name to the loop.  It
;;; translates to a block name, so that RETURN-FROM can be used to
;;; exit the loop.  By default, the name of the loop is nil.
;;;
;;; The name-clause is optional, and if present, must be the first one
;;; in the body.  The syntax is:
;;;
;;;    NAMED name
;;;
;;; where name is a symbol.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser.

(define-parser name-clause-parser
  (consecutive (lambda (named name)
                 (let ((name name)
                       (bound-variables (constantly '()))
                       (accumulation-variables (constantly '())))
                   (curlet)))
               (keyword-parser 'named)
               (singleton identity symbol?)))

(add-clause-parser name-clause-parser)
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

(defclass with-subclause-no-form (with-subclause) ()
  (wrap-subclause (subclause inner-form)
    (let* ((vars-and-types
             (extract-variables (subclause 'var-spec) (subclause 'type-spec)))
           (vars-and-values
             (map (lambda (vt)
                    (list (car vt) (case (cadr vt)
                                     ((fixnum) 0)
                                     ((float) 0.0)
                                     (else #<undefined>))))
                  vars-and-types))) ;undefined was nil in cl
      `(let ,vars-and-values
         ,inner-form))))

(defclass with-subclause-with-form (with-subclause)
  (form
   (form-var (gensym)))

  (initial-bindings (clause)
    `((,(clause 'form-var) ,(clause 'form))))

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
                 (pidgin-destructuring-bind (temp-vars dictionary)
                      (fresh-variables var-spec)
                    (make-instance 'with-subclause-with-form
                      :var-spec var-spec
                      :type-spec type-spec
                      :temp-vars temp-vars
                      :dictionary dictionary
                      :form form)))
               ;; Accept anything for now.  Analyze later.
               anything-parser
               optional-type-spec-parser
               (keyword-parser '=)
               anything-parser))

;;; Parser for var [type-spec]
(define-parser with-subclause-type-2-parser
  (consecutive (lambda (var-spec type-spec)
                 (pidgin-destructuring-bind (temp-vars dictionary)
                      (fresh-variables var-spec)
                   (make-instance 'with-subclause-no-form
                     :var-spec var-spec
                     :type-spec type-spec
                     :temp-vars temp-vars
                     :dictionary dictionary)))
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
                 (make-instance 'with-clause
                   :subclauses (cons first rest)))
               (keyword-parser 'with)
               with-subclause-no-keyword-parser
               (repeat* list
                        with-subclause-and-parser)))

(add-clause-parser with-clause-parser)
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
;;;; Clause DO-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DO-CLAUSE.
;;;
;;; An DO clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    do-clause ::= do compound-form+

(defclass do-clause (unconditional-clause)
  (body)
  (accumulation-variables (clause)
    '())

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the body-form.
  (body-form (clause end-tag)
    (clause 'body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser do-clause-parser
  (consecutive (lambda (do compound+)
                 (make-instance 'do-clause
                   :body compound+))
               (alternative (keyword-parser 'do)
                            (keyword-parser 'doing))
               compound+))

(add-clause-parser do-clause-parser)
(defclass collect-clause (list-accumulation-clause) ())

(defclass collect-it-clause (collect-clause it-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*list-tail-accumulation-variable*
                      (list ,*it-var*))
                (set! ,*accumulation-variable*
                      ,*list-tail-accumulation-variable*))
         (begin (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))
                (set-cdr! ,*list-tail-accumulation-variable*
                          (list ,*it-var*))))))

(defclass collect-form-clause (collect-clause form-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
       (begin (set! ,*list-tail-accumulation-variable*
                    (list-values ,(clause 'form)))
              (set! ,*accumulation-variable*
                    ,*list-tail-accumulation-variable*))
       (begin (set! ,*list-tail-accumulation-variable*
                    (,last ,*list-tail-accumulation-variable*))
              (set-cdr! ,*list-tail-accumulation-variable*
                        (list-values ,(clause 'form)))))))

(defclass collect-it-into-clause (into-mixin collect-clause it-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(tail-variable (clause 'into-var))
                      (list ,*it-var*))
                (set! ,(clause 'into-var)
                      ,(tail-variable (clause 'into-var))))
         (begin (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))
                (set-cdr! ,(tail-variable (clause 'into-var))
                          (list ,*it-var*))))))

(defclass collect-form-into-clause (into-mixin collect-clause form-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
       (begin (set! ,(tail-variable (clause 'into-var))
                    (list-values ,(clause 'form)))
              (set! ,(clause 'into-var)
                    ,(tail-variable (clause 'into-var))))
       (begin (set! ,(tail-variable (clause 'into-var))
                    (,last ,(tail-variable (clause 'into-var))))
              (set-cdr! ,(tail-variable (clause 'into-var))
                        (list-values ,(clause 'form)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser collect-it-into-clause-parser
  (consecutive (lambda (collect it into var)
                 (make-instance 'collect-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'collect)
                            (keyword-parser 'collecting))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity
                          symbol?)))

(define-parser collect-it-clause-parser
  (consecutive (lambda (collect it)
                 (make-instance 'collect-it-clause))
               (alternative (keyword-parser 'collect)
                            (keyword-parser 'collecting))
               (keyword-parser 'it)))

(define-parser collect-form-into-clause-parser
  (consecutive (lambda (collect form into var)
                 (make-instance 'collect-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'collect)
                            (keyword-parser 'collecting))
               anything-parser
               (keyword-parser 'into)
               (singleton identity
                          symbol?)))

(define-parser collect-form-clause-parser
  (consecutive (lambda (collect form)
                 (make-instance 'collect-form-clause
                   :form form))
               (alternative (keyword-parser 'collect)
                            (keyword-parser 'collecting))
               anything-parser))

(define-parser collect-clause-parser
  (alternative collect-it-into-clause-parser
               collect-it-clause-parser
               collect-form-into-clause-parser
               collect-form-clause-parser))

(add-clause-parser collect-clause-parser)
(defclass append-clause (list-accumulation-clause) ())

(defclass append-it-clause (append-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*accumulation-variable*
                      (,copy-list ,*it-var*))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))
                (set-cdr! ,*list-tail-accumulation-variable*
                        (,copy-list ,*it-var*))))))

(defclass append-form-clause (append-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*accumulation-variable*
                      (,copy-list ,(clause 'form)))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))
                (set-cdr! ,*list-tail-accumulation-variable*
                        (,copy-list ,(clause 'form)))))))

(defclass append-it-into-clause (into-mixin append-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(clause 'into-var)
                      (,copy-list ,*it-var*))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(clause 'into-var))))
         (begin (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))
                (set-cdr! ,(tail-variable (clause 'into-var))
                        (,copy-list ,*it-var*))))))

(defclass append-form-into-clause (into-mixin append-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(clause 'into-var)
                      (,copy-list ,(clause 'form)))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(clause 'into-var))))
         (begin (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))
                (set-cdr! ,(tail-variable (clause 'into-var))
                        (,copy-list ,(clause 'form)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser append-it-into-clause-parser
  (consecutive (lambda (append it into var)
                 (make-instance 'append-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity symbol?)))

(define-parser append-it-clause-parser
  (consecutive (lambda (append it)
                 (make-instance 'append-it-clause))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               (keyword-parser 'it)))

(define-parser append-form-into-clause-parser
  (consecutive (lambda (append form into var)
                 (make-instance 'append-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               anything-parser
               (keyword-parser 'into)
               (singleton identity symbol?)))

(define-parser append-form-clause-parser
  (consecutive (lambda (append form)
                 (make-instance 'append-form-clause
                   :form form))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               anything-parser))

(define-parser append-clause-parser
  (alternative append-it-into-clause-parser
               append-it-clause-parser
               append-form-into-clause-parser
               append-form-clause-parser))

(add-clause-parser append-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.




(defclass nconc-clause (list-accumulation-clause) ())

(defclass nconc-it-clause (nconc-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*accumulation-variable*
                      ,*it-var*)
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))
                (set-cdr! ,*list-tail-accumulation-variable*
                        ,*it-var*)))))

(defclass nconc-form-clause (nconc-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*accumulation-variable*
                      ,(clause 'form))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))
                (set-cdr! ,*list-tail-accumulation-variable*
                        ,(clause 'form))))))

(defclass nconc-it-into-clause (into-mixin nconc-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(clause 'into-var)
                      ,*it-var*)
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(clause 'into-var))))
         (begin (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))
                (set-cdr! ,(tail-variable (clause 'into-var))
                        ,*it-var*)))))

(defclass nconc-form-into-clause (into-mixin nconc-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(clause 'into-var)
                      ,(clause 'form))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(clause 'into-var))))
         (begin (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))
                (set-cdr! ,(tail-variable (clause 'into-var))
                          ,(clause 'form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser nconc-it-into-clause-parser
  (consecutive (lambda (nconc it into var)
                 (make-instance 'nconc-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity symbol?)))

(define-parser nconc-it-clause-parser
  (consecutive (lambda (nconc it)
                 (make-instance 'nconc-it-clause))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               (keyword-parser 'it)))

(define-parser nconc-form-into-clause-parser
  (consecutive (lambda (nconc form into var)
                 (make-instance 'nconc-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               anything-parser
               (keyword-parser 'into)
               (singleton identity symbol?)))

(define-parser nconc-form-clause-parser
  (consecutive (lambda (nconc form)
                 (make-instance 'nconc-form-clause
                   :form form))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               anything-parser))

(define-parser nconc-clause-parser
  (alternative nconc-it-into-clause-parser
               nconc-it-clause-parser
               nconc-form-into-clause-parser
               nconc-form-clause-parser))

(add-clause-parser nconc-clause-parser)
(defclass count-clause (count/sum-accumulation-clause) ())

(defclass count-it-clause (count-clause it-mixin) ()
  (body-form (clause end-tag)
    `(when ,*it-var*
       (set! ,*accumulation-variable*
             (+ 1 ,*accumulation-variable*)))))

(defclass count-form-clause (count-clause form-mixin) ()
  (body-form (clause end-tag)
    `(when ,(clause 'form)
       (set! ,*accumulation-variable*
             (+ 1 ,*accumulation-variable*)))))

(defclass count-it-into-clause (into-mixin count-clause it-mixin) ()
  (body-form (clause end-tag)
    `(when ,*it-var*
       (set! ,(clause 'into-var)
             (+ 1 ,(clause 'into-var))))))

(defclass count-form-into-clause (into-mixin count-clause form-mixin) ()
  (body-form (clause end-tag)
    `(when ,(clause 'form)
       (set! ,(clause 'into-var)
             (+ 1 ,(clause 'into-var))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser count-it-into-clause-parser
  (consecutive (lambda (count it into var type-spec)
                 (make-instance 'count-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity
                          symbol?)
               optional-type-spec-parser))

(define-parser count-it-clause-parser
  (consecutive (lambda (count it type-spec)
                 (make-instance 'count-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               (keyword-parser 'it)
               optional-type-spec-parser))

(define-parser count-form-into-clause-parser
  (consecutive (lambda (count form into var type-spec)
                 (make-instance 'count-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               anything-parser
               (keyword-parser 'into)
               (singleton identity
                          symbol?)
               optional-type-spec-parser))

(define-parser count-form-clause-parser
  (consecutive (lambda (count form type-spec)
                 (make-instance 'count-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               anything-parser
               optional-type-spec-parser))

(define-parser count-clause-parser
  (alternative count-it-into-clause-parser
               count-it-clause-parser
               count-form-into-clause-parser
               count-form-clause-parser))

(add-clause-parser count-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.
(defclass sum-clause (count/sum-accumulation-clause) ())

(defclass sum-it-clause (sum-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
           (,sum ,*accumulation-variable* ,*it-var*))))

(defclass sum-form-clause (sum-clause form-mixin) ()
  (body-form (clause end-tag)
      `(set! ,*accumulation-variable*
         (apply ,sum ,*accumulation-variable* (list-values ,(clause 'form))))))

(defclass sum-it-into-clause (into-mixin sum-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (,sum ,(clause 'into-var) ,*it-var*))))

(defclass sum-form-into-clause (into-mixin sum-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (apply ,sum ,(clause 'into-var) (list-values ,(clause 'form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser sum-it-into-clause-parser
  (consecutive (lambda (sum it into var type-spec)
                 (make-instance 'sum-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity
                          symbol?)
               optional-type-spec-parser))

(define-parser sum-it-clause-parser
  (consecutive (lambda (sum it type-spec)
                 (make-instance 'sum-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               (keyword-parser 'it)
               optional-type-spec-parser))

(define-parser sum-form-into-clause-parser
  (consecutive (lambda (sum form into var type-spec)
                 (make-instance 'sum-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               anything-parser
               (keyword-parser 'into)
               (singleton identity
                          symbol?)
               optional-type-spec-parser))

(define-parser sum-form-clause-parser
  (consecutive (lambda (sum form type-spec)
                 (make-instance 'sum-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               anything-parser
               optional-type-spec-parser))

(define-parser sum-clause-parser
  (alternative sum-it-into-clause-parser
               sum-it-clause-parser
               sum-form-into-clause-parser
               sum-form-clause-parser))

(add-clause-parser sum-clause-parser)
(defclass maximize-clause (numeric-accumulation-clause) ((accumulation-category 'max)))

(defclass maximize-it-clause (maximize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
           (,maximize ,*accumulation-variable* ,*it-var*))))

(defclass maximize-form-clause (maximize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
           (apply ,maximize ,*accumulation-variable* (list-values ,(clause 'form))))))

(defclass maximize-it-into-clause (into-mixin maximize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (,maximize ,(clause 'into-var) ,*it-var*))))

(defclass maximize-form-into-clause (into-mixin maximize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (apply ,maximize ,(clause 'into-var) (list-values ,(clause 'form))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser maximize-it-into-clause-parser
  (consecutive (lambda (maximize it into var type-spec)
                 (make-instance 'maximize-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity symbol?)
               optional-type-spec-parser))

(define-parser maximize-it-clause-parser
  (consecutive (lambda (maximize it type-spec)
                 (make-instance 'maximize-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               (keyword-parser 'it)
               optional-type-spec-parser))

(define-parser maximize-form-into-clause-parser
  (consecutive (lambda (maximize form into var type-spec)
                 (make-instance 'maximize-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               anything-parser
               (keyword-parser 'into)
               (singleton identity symbol?)
               optional-type-spec-parser))

(define-parser maximize-form-clause-parser
  (consecutive (lambda (maximize form type-spec)
                 (make-instance 'maximize-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               anything-parser
               optional-type-spec-parser))

(define-parser maximize-clause-parser
  (alternative maximize-it-into-clause-parser
               maximize-it-clause-parser
               maximize-form-into-clause-parser
               maximize-form-clause-parser))

(add-clause-parser maximize-clause-parser)
(defclass minimize-clause (numeric-accumulation-clause) ((accumulation-category 'min)))

(defclass minimize-it-clause (minimize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
          (,minimize ,*accumulation-variable* ,*it-var*))))

(defclass minimize-form-clause (minimize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
          (apply ,minimize ,*accumulation-variable* (list-values ,(clause 'form))))))

(defclass minimize-it-into-clause (into-mixin minimize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
          (,minimize ,(clause 'into-var) ,*it-var*))))

(defclass minimize-form-into-clause (into-mixin minimize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
          (apply ,minimize ,(clause 'into-var) (list-values ,(clause 'form))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-it-into-clause-parser
  (consecutive (lambda (minimize it into var type-spec)
                 (make-instance 'minimize-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'minimize)
                            (keyword-parser 'minimizing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity symbol?)
               optional-type-spec-parser))

(define-parser minimize-it-clause-parser
  (consecutive (lambda (minimize it type-spec)
                 (make-instance 'minimize-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'minimize)
                            (keyword-parser 'minimizing))
               (keyword-parser 'it)
               optional-type-spec-parser))

(define-parser minimize-form-into-clause-parser
  (consecutive (lambda (minimize form into var type-spec)
                 (make-instance 'minimize-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'minimize)
                            (keyword-parser 'minimizing))
               anything-parser
               (keyword-parser 'into)
               (singleton identity symbol?)
               optional-type-spec-parser))

(define-parser minimize-form-clause-parser
  (consecutive (lambda (minimize form type-spec)
                 (make-instance 'minimize-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'minimize)
                            (keyword-parser 'minimizing))
               anything-parser
               optional-type-spec-parser))

(define-parser minimize-clause-parser
  (alternative minimize-it-into-clause-parser
               minimize-it-clause-parser
               minimize-form-into-clause-parser
               minimize-form-clause-parser))

(add-clause-parser minimize-clause-parser)
(defclass conditional-clause (selectable-clause)
  (condition then-clauses else-clauses)

  ;;; A conditional clause does not introduce any bindings for any
  ;;; variables, so this method should return the empty list.
  (bound-variables (clause)
    '())

  (accumulation-variables (clause)
    (append (apply append
                    (map accumulation-variables (clause 'then-clauses)))
            (apply append
                    (map accumulation-variables (clause 'else-clauses)))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute body-form.

  (body-form (clause end-tag)
    (let-temporarily ((*it-var* (gensym)))
      `(let ((,*it-var* ,(clause 'condition)))
         (if ,*it-var*
             (begin
               ,@(map (lambda (clause)
                           (body-form clause end-tag))
                         (clause 'then-clauses)))
             (begin
               ,@(map (lambda (clause)
                           (body-form clause end-tag))
                         (clause 'else-clauses))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser then-or-else-parser
  (consecutive cons
               selectable-clause-parser
               (repeat* list
                        and-selectable-clause-parser)))
(define-parser if-else-end-clause-parser
  (consecutive (lambda (if form then-clauses else else-clauses end)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser
               (keyword-parser 'else)
               then-or-else-parser
               (keyword-parser 'end)))

(define-parser if-end-clause-parser
  (consecutive (lambda (if form then-clauses end)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses '()))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser
               (keyword-parser 'end)))

(define-parser if-else-clause-parser
  (consecutive (lambda (if form then-clauses else else-clauses)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser
               (keyword-parser 'else)
               then-or-else-parser))

(define-parser if-clause-parser
  (consecutive (lambda (if form then-clauses)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses '()))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser))

(define-parser if-when-parser
  (alternative if-else-end-clause-parser
               if-end-clause-parser
               if-else-clause-parser
               if-clause-parser))

(define-parser unless-else-end-clause-parser
  (consecutive (lambda (unless form else-clauses else then-clauses end)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               anything-parser
               then-or-else-parser
               (keyword-parser 'else)
               then-or-else-parser
               (keyword-parser 'end)))

(define-parser unless-end-clause-parser
  (consecutive (lambda (unless form else-clauses end)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses '()
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               anything-parser
               then-or-else-parser
               (keyword-parser 'end)))

(define-parser unless-else-clause-parser
  (consecutive (lambda (unless form else-clauses else then-clauses)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               anything-parser
               then-or-else-parser
               (keyword-parser 'else)
               then-or-else-parser))

(define-parser unless-clause-parser
  (consecutive (lambda (unless form else-clauses)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses '()
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'unless)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser))

(define-parser unless-parser
  (alternative unless-else-end-clause-parser
               unless-end-clause-parser
               unless-else-clause-parser
               unless-clause-parser))

(define-parser conditional-clause-parser
  (alternative if-when-parser
               unless-parser))

(add-clause-parser conditional-clause-parser)

(defclass while-clause (termination-test-clause)
  (form)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the body-form

  (body-form (clause end-tag)
    `(unless ,(clause 'form)
       (,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser while-clause-parser
  (consecutive (lambda (while form)
                 (make-instance 'while-clause
                   :form form))
               (keyword-parser 'while)
               anything-parser))

(add-clause-parser while-clause-parser)

(define-parser until-clause-parser
  (consecutive (lambda (until form)
                 (make-instance 'while-clause
                   :form `(not ,form)))
               (keyword-parser 'until)
               anything-parser))

(add-clause-parser until-clause-parser)
(defclass repeat-clause (termination-test-clause var-and-type-spec-mixin)
  (form
   (var-spec (gensym))
   (type-spec 'real))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the bindings.

  (initial-bindings (clause)
    `((,(clause 'var-spec) ,(clause 'form))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the declarations.

  (declarations (clause)
    '())

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the prologue-form.

  (prologue-form (clause end-tag)
    `(when (<= ,(clause 'var-spec) 0)
       (,end-tag)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the termination-form.

  (termination-form (clause end-tag)
    `(when (<= ,(clause 'var-spec) 1)
       (,end-tag)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the step-form.

  (step-form (clause)
    `(set! ,(clause 'var-spec) (- ,(clause 'var-spec) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser repeat-clause-parser
  (consecutive (lambda (repeat form)
                 (make-instance 'repeat-clause :form form))
               (keyword-parser 'repeat)
               anything-parser))

(add-clause-parser repeat-clause-parser)
(defclass always-clause (termination-test-clause form-mixin) ()
  (accumulation-variables (clause)
    `((#f always/never t)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the body-form

  (body-form (clause end-tag)
    `(unless ,(clause 'form)
       (,*loop-return-sym* #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser always-clause-parser
  (consecutive (lambda (always form)
                 (make-instance 'always-clause
                   :form form))
               (keyword-parser 'always)
               anything-parser))

(add-clause-parser always-clause-parser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser never-clause-parser
  (consecutive (lambda (never form)
                 (make-instance 'always-clause
                   :form (list not form)))
               (keyword-parser 'never)
               anything-parser))

(add-clause-parser never-clause-parser)
(defclass thereis-clause (termination-test-clause form-mixin) ()
  (accumulation-variables (clause)
    `((nil thereis t)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the body-form

  (body-form (clause end-tag)
    `(let ((temp ,(clause 'form)))
       (when temp
         (,*loop-return-sym* temp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser thereis-clause-parser
  (consecutive (lambda (thereis form)
                 (make-instance 'thereis-clause
                   :form form))
               (keyword-parser 'thereis)
               anything-parser))

(add-clause-parser thereis-clause-parser)
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
    (apply append (map initial-bindings (clause 'subclauses))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ARITHMETIC.

(defclass for-as-arithmetic (for-as-subclause var-and-type-spec-mixin)
  (;; The order in which the forms are given.  This is a list of three
   ;; elements FROM, TO, and BY in the order that they were given in
   ;; the clause.
   order
   ;; The form that was given after one of the LOOP keywords FROM,
   ;; UPFROM, or DOWNFROM, or 0 if none of these LOOP keywords was
   ;; given.
   (start-form 0)
   (start-var (gensym))
   ;; The form that was after one of the LOOP keywords TO, UPTO,
   ;; DOWNTO, BELOW, or ABOVE, or NIL if none of these LOOP keywords
   ;; was given.
   (end-form '())
   (end-var (gensym))
   ;; The form that was after the LOOP keyword BY, or 0 if this
   ;; keyword was not given.
   (by-form 1)
   (by-var (gensym))
   ;; If termination is TO, UPTO, or DOWNTO, then this slot contains
   ;; the symbol <=.  If termination is ABOVE or BELOW, then this slot
   ;; contains the symbol <.  If there is TO/UPTO/DOWNTO/ABOVE/BELOW,
   ;; then the loop does not terminate because of this clause, and
   ;; then this slot contains NIL.
   (termination-test '())
   ;; This variable is one step ahead of the iteration variable, and
   ;; when the iteration variable is NIL, the value of this variable
   ;; is never assigned to any iteration variable.
   (temp-var (gensym)))

  (bound-variables (subclause)
    (map car
         (extract-variables (subclause 'var-spec) #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the bindings.

  (initial-bindings (clause)
    (let ((order (clause 'order)))
      (cond ((equal? order '(from to by))
             `((,(clause 'start-var) ,(clause 'start-form))
               ,@(if (null? (clause 'end-form))
                     '()
                     `((,(clause 'end-var) ,(clause 'end-form))))
               (,(clause 'by-var) ,(clause 'by-form))))
            ((equal? order '(from by to))
             `((,(clause 'start-var) ,(clause 'start-form))
               (,(clause 'by-var) ,(clause 'by-form))
               ,@(if (null? (clause 'end-form))
                     '()
                     `((,(clause 'end-var) ,(clause 'end-form))))))
            ((equal? order '(to from by))
             `(,@(if (null? (clause 'end-form))
                     '()
                     `((,(clause 'end-var) ,(clause 'end-form))))
               (,(clause 'start-var) ,(clause 'start-form))
               (,(clause 'by-var) ,(clause 'by-form))))
            ((equal? order '(to by from))
             `(,@(if (null? (clause 'end-form))
                     '()
                     `((,(clause 'end-var) ,(clause 'end-form))))
               (,(clause 'by-var) ,(clause 'by-form))
               (,(clause 'start-var) ,(clause 'start-form))))
            ((equal? order '(by from to))
             `((,(clause 'by-var) ,(clause 'by-form))
               (,(clause 'start-var) ,(clause 'start-form))
               ,@(if (null? (clause 'end-form))
                     '()
                     `((,(clause 'end-var) ,(clause 'end-form))))))
            ((equal? order '(by to from))
             `((,(clause 'by-var) ,(clause 'by-form))
               ,@(if (null? (clause 'end-form))
                     '()
                     `((,(clause 'end-var) ,(clause 'end-form))))
               (,(clause 'start-var) ,(clause 'start-form)))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute subclause wrapping.

  (wrap-subclause (subclause inner-form)
    (if (null? (subclause 'var-spec))
        `(let ((,(subclause 'temp-var) ,(subclause 'start-var)))
           ,inner-form)
        `(let ((,(subclause 'temp-var) ,(subclause 'start-var))
               (,(subclause 'var-spec) ,(subclause 'start-var)))
           ;(declare (cl:type ,(type-spec subclause) ,(var-spec subclause)))
           ,inner-form))))

(defclass for-as-arithmetic-up (for-as-arithmetic) ()
  (prologue-form (clause end-tag)
    (if (null? (clause 'termination-test))
        `(set! ,(clause 'temp-var) (+ ,(clause 'temp-var),(clause 'by-var)))
        `(if (,(clause 'termination-test)
              ,(clause 'temp-var)
              ,(clause 'end-var))
             (set! ,(clause 'temp-var) (+ ,(clause 'temp-var) ,(clause 'by-var)))
             (,end-tag))))

  (termination-form (clause end-tag)
    (if (null? (clause 'termination-test))
        '()
        `(unless (,(clause 'termination-test)
                  ,(clause 'temp-var)
                  ,(clause 'end-var))
           (,end-tag))))

  (step-form (clause)
    (if (null? (clause 'var-spec))
        `(set! ,(clause 'temp-var) (+ ,(clause 'temp-var) ,(clause 'by-var)))
        `(begin (set! ,(clause 'var-spec) ,(clause 'temp-var))
                (set! ,(clause 'temp-var) (+ ,(clause 'temp-var) ,(clause 'by-var)))))))

(defclass for-as-arithmetic-down (for-as-arithmetic) ()
  (prologue-form (clause end-tag)
    (if (null? (clause 'termination-test))
        `(set! ,(clause 'temp-var) (- ,(clause 'temp-var) ,(clause 'by-var)))
        `(if (,(clause 'termination-test)
              ,(clause 'end-var)
              ,(clause 'temp-var))
             (set! ,(clause 'temp-var) (- ,(clause 'temp-var) ,(clause 'by-var)))
             (,end-tag))))

  (termination-form (clause end-tag)
    (if (null? (clause 'termination-test))
      '()
       `(unless (,(clause 'termination-test)
                 ,(clause 'end-var)
                 ,(clause 'temp-var))
          (,end-tag))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the step-form.


  (step-form (clause)
    (if (null? (clause 'var-spec))
        `(set! ,(clause 'temp-var) (- ,(clause 'temp-var) ,(clause 'by-var)))
        `(begin (set! ,(clause 'var-spec) ,(clause 'temp-var))
                (set! ,(clause 'temp-var) (- ,(clause 'temp-var) ,(clause 'by-var)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a TYPE-SPEC determine a value used for variable
;;; initialization and a type to use in a declaration, and return them
;;; as two values.  The type returned may be different from the
;;; TYPE-SPEC argument because we may not be able to determine a
;;; initialization value that would conform to the TYPE-SPEC, and in
;;; that case, we must modify the type so that it covers the
;;; initialization value that we give.
;;;
;;; Perhaps this code should be moved to the code utilities module.

(define (arithmetic-value-and-type type-spec)
  (cond ((eq? type-spec 'fixnum)
         (list 0 type-spec))
        ((eq? type-spec 'float)
         (list 0.0 type-spec))
        ;; We could add some more here, for instance intervals
        ;; of floats.
        (#t
         (list 0 #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for simple variable.

(define-parser simple-var-parser
  (singleton identity
             (lambda (x)
               (or (null? x)
                   (symbol? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers for individual keywords.

(define (project-form keyword form)
  form)

(define-parser from-parser
  (consecutive project-form (keyword-parser 'from) anything-parser))

(define-parser upfrom-parser
  (consecutive project-form (keyword-parser 'upfrom) anything-parser))

(define-parser downfrom-parser
  (consecutive project-form (keyword-parser 'downfrom) anything-parser))

(define-parser to-parser
  (consecutive (lambda (keyword form)
                 (cons '<= form))
               (keyword-parser 'to)
               anything-parser))

(define-parser upto-parser
  (consecutive (lambda (keyword form)
                 (cons '<= form))
               (keyword-parser 'upto)
               anything-parser))

(define-parser below-parser
  (consecutive (lambda (keyword form)
                 (cons '< form))
               (keyword-parser 'below)
               anything-parser))

(define-parser downto-parser
  (consecutive (lambda (keyword form)
                 (cons '<= form))
               (keyword-parser 'downto)
               anything-parser))

(define-parser above-parser
  (consecutive (lambda (keyword form)
                 (cons '< form))
               (keyword-parser 'above)
               anything-parser))

(define-parser by-parser
  (consecutive project-form (keyword-parser 'by) anything-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers for arithmetic up.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where FROM/UPFROM TO/UPTO/BELOW and BY are all present.
;;; Since they can appear in any order, there are 6 different
;;; variations.

;;; Order is FROM TO BY.
(define-parser arithmetic-up-1-parser
  (consecutive (lambda (var type-spec from to by)
                 (make-instance 'for-as-arithmetic-up
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative from-parser upfrom-parser)
               (alternative to-parser upto-parser below-parser)
               by-parser))

;;; Order is FROM BY TO.
(define-parser arithmetic-up-2-parser
  (consecutive (lambda (var type-spec from by to)
                 (make-instance 'for-as-arithmetic-up
                   :order '(from by to)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative from-parser upfrom-parser)
               by-parser
               (alternative to-parser upto-parser below-parser)))

;;; Order is TO FROM BY.
(define-parser arithmetic-up-3-parser
  (consecutive (lambda (var type-spec to from by)
                 (make-instance 'for-as-arithmetic-up
                   :order '(to from by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative to-parser upto-parser below-parser)
               (alternative from-parser upfrom-parser)
               by-parser))

;;; Order is TO BY FROM.
(define-parser arithmetic-up-4-parser
  (consecutive (lambda (var type-spec to by from)
                 (make-instance 'for-as-arithmetic-up
                   :order '(to by from)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative to-parser upto-parser below-parser)
               by-parser
               (alternative from-parser upfrom-parser)))

;;; Order is BY FROM TO.
(define-parser arithmetic-up-5-parser
  (consecutive (lambda (var type-spec by from to)
                 (make-instance 'for-as-arithmetic-up
                   :order '(by from to)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               by-parser
               (alternative from-parser upfrom-parser)
               (alternative to-parser upto-parser below-parser)))

;;; Order is BY TO FROM.
(define-parser arithmetic-up-6-parser
  (consecutive (lambda (var type-spec by to from)
                 (make-instance 'for-as-arithmetic-up
                   :order '(by to from)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               by-parser
               (alternative to-parser upto-parser below-parser)
               (alternative from-parser upfrom-parser)))

(define-parser three-keyword-up-parser
  (alternative arithmetic-up-1-parser
               arithmetic-up-2-parser
               arithmetic-up-3-parser
               arithmetic-up-4-parser
               arithmetic-up-5-parser
               arithmetic-up-6-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where only FROM/UPFROM and TO/UPTO/BELOW appear (BY is
;;; omitted).  Since they can appear in any order, there are 2
;;; different variations.

;;; Order is FROM TO.
(define-parser arithmetic-up-7-parser
  (consecutive (lambda (var type-spec from to)
                 (make-instance 'for-as-arithmetic-up
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative from-parser upfrom-parser)
               (alternative to-parser upto-parser below-parser)))

;;; Order is TO FROM.
(define-parser arithmetic-up-8-parser
  (consecutive (lambda (var type-spec to from)
                 (make-instance 'for-as-arithmetic-up
                   :order '(to from by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative to-parser upto-parser below-parser)
               (alternative from-parser upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where only FROM/UPFROM and BY appear (TO/UPTO/BELOW is
;;; omitted).  Since they can appear in any order, there are 2
;;; different variations.

;;; Order is FROM BY.
(define-parser arithmetic-up-9-parser
  (consecutive (lambda (var type-spec from by)
                 (make-instance 'for-as-arithmetic-up
                   :order '(from by to)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :by-form by))
               simple-var-parser
               optional-type-spec-parser
               (alternative from-parser upfrom-parser)
               by-parser))

;;; Order is BY FROM.
(define-parser arithmetic-up-10-parser
  (consecutive (lambda (var type-spec by from)
                 (make-instance 'for-as-arithmetic-up
                   :order '(by from to)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :by-form by))
               simple-var-parser
               optional-type-spec-parser
               by-parser
               (alternative from-parser upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where only TO/UPTO/BELOW and BY appear (FROM/UPFROM is
;;; omitted).  Since they can appear in any order, there are 2
;;; different variations.

;;; Order is TO BY.
(define-parser arithmetic-up-11-parser
  (consecutive (lambda (var type-spec to by)
                 (make-instance 'for-as-arithmetic-up
                   :order '(to by from)
                   :var-spec var
                   :type-spec type-spec
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative to-parser upto-parser below-parser)
               by-parser))

;;; Order is BY TO.
(define-parser arithmetic-up-12-parser
  (consecutive (lambda (var type-spec by to)
                 (make-instance 'for-as-arithmetic-up
                   :order '(by to from)
                   :var-spec var
                   :type-spec type-spec
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               by-parser
               (alternative to-parser upto-parser below-parser)))

(define-parser two-keyword-up-parser
  (alternative arithmetic-up-7-parser
               arithmetic-up-8-parser
               arithmetic-up-9-parser
               arithmetic-up-10-parser
               arithmetic-up-11-parser
               arithmetic-up-12-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only FROM/UPFROM appears (TO/UPTO/BELOW and BY are
;;; omitted).

(define-parser arithmetic-up-13-parser
  (consecutive (lambda (var type-spec from)
                 (make-instance 'for-as-arithmetic-up
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from))
               simple-var-parser
               optional-type-spec-parser
               (alternative from-parser upfrom-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only TO/UPTO/BELOW appears (FROM/UPFROM and BY are
;;; omitted).

(define-parser arithmetic-up-14-parser
  (consecutive (lambda (var type-spec to)
                 (make-instance 'for-as-arithmetic-up
                   :order '(to from by)
                   :var-spec var
                   :type-spec type-spec
                   :end-form (cdr to)
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative to-parser upto-parser below-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser where only BY appears (FROM/UPFROM and TO/UPTO/BELOW are
;;; omitted).

(define-parser arithmetic-up-15-parser
  (consecutive (lambda (var type-spec by)
                 (make-instance 'for-as-arithmetic-up
                   :order '(by to from)
                   :var-spec var
                   :type-spec type-spec
                   :by-form by))
               simple-var-parser
               optional-type-spec-parser
               by-parser))

(define-parser one-keyword-up-parser
  (alternative arithmetic-up-13-parser
               arithmetic-up-14-parser
               arithmetic-up-15-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers for arithmetic down.
;;;
;;; There is no default start value for decremental stepping, so
;;; either FROM or DOWNFROM must always be supplied.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers where FROM/DOWNFROM TO/DOWNTO/ABOVE and BY are all present.
;;;
;;; The combination FROM - TO is not allowed.

;;; FROM/DOWNFROM - DOWNTO/ABOVE - BY
(define-parser arithmetic-down-1-parser
  (consecutive (lambda (var type-spec from to by)
                 (make-instance 'for-as-arithmetic-down
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative from-parser downfrom-parser)
               (alternative downto-parser above-parser)
               by-parser))

;;; FROM/DOWNFROM - BY - DOWNTO/ABOVE
(define-parser arithmetic-down-2-parser
  (consecutive (lambda (var type-spec from by to)
                 (make-instance 'for-as-arithmetic-down
                   :order '(from by to)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative from-parser downfrom-parser)
               by-parser
               (alternative downto-parser above-parser)))

;;; DOWNTO/ABOVE - FROM/DOWNFROM - BY
(define-parser arithmetic-down-3-parser
  (consecutive (lambda (var type-spec to from by)
                 (make-instance 'for-as-arithmetic-down
                   :order '(to from by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative downto-parser above-parser)
               (alternative from-parser downfrom-parser)
               by-parser))

;;; DOWNTO/ABOVE - BY - FROM/DOWNFROM
(define-parser arithmetic-down-4-parser
  (consecutive (lambda (var type-spec to by from)
                 (make-instance 'for-as-arithmetic-down
                   :order '(to by from)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative downto-parser above-parser)
               by-parser
               (alternative from-parser downfrom-parser)))

;;; BY- FROM/DOWNFROM - DOWNTO/ABOVE
(define-parser arithmetic-down-5-parser
  (consecutive (lambda (var type-spec by from to)
                 (make-instance 'for-as-arithmetic-down
                   :order '(by from to)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               by-parser
               (alternative from-parser downfrom-parser)
               (alternative downto-parser above-parser)))

;;; BY- DOWNTO/ABOVE - FROM/DOWNFROM
(define-parser arithmetic-down-6-parser
  (consecutive (lambda (var type-spec by to from)
                 (make-instance 'for-as-arithmetic-down
                   :order '(by to from)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               by-parser
               (alternative downto-parser above-parser)
               (alternative from-parser downfrom-parser)))

;;; DOWNFROM - TO/DOWNTO/ABOVE - BY
(define-parser arithmetic-down-7-parser
  (consecutive (lambda (var type-spec from to by)
                 (make-instance 'for-as-arithmetic-down
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               downfrom-parser
               (alternative to-parser downto-parser above-parser)
               by-parser))

;;; DOWNFROM - BY - TO/DOWNTO/ABOVE
(define-parser arithmetic-down-8-parser
  (consecutive (lambda (var type-spec from by to)
                 (make-instance 'for-as-arithmetic-down
                   :order '(from by to)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               downfrom-parser
               by-parser
               (alternative to-parser downto-parser above-parser)))

;;; TO/DOWNTO/ABOVE - DOWNFROM - BY
(define-parser arithmetic-down-9-parser
  (consecutive (lambda (var type-spec to from by)
                 (make-instance 'for-as-arithmetic-down
                   :order '(to from by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative to-parser downto-parser above-parser)
               downfrom-parser
               by-parser))

;;; TO/DOWNTO/ABOVE - BY - DOWNFROM
(define-parser arithmetic-down-10-parser
  (consecutive (lambda (var type-spec to by from)
                 (make-instance 'for-as-arithmetic-down
                   :order '(to by from)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative to-parser downto-parser above-parser)
               by-parser
               downfrom-parser))

;;; BY- DOWNFROM - TO/DOWNTO/ABOVE
(define-parser arithmetic-down-11-parser
  (consecutive (lambda (var type-spec by from to)
                 (make-instance 'for-as-arithmetic-down
                   :order '(by from to)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               by-parser
               downfrom-parser
               (alternative to-parser downto-parser above-parser)))

;;; BY- TO/DOWNTO/ABOVE - DOWNFROM
(define-parser arithmetic-down-12-parser
  (consecutive (lambda (var type-spec by to from)
                 (make-instance 'for-as-arithmetic-down
                   :order '(by to from)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :by-form by
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               by-parser
               (alternative to-parser downto-parser above-parser)
               downfrom-parser))

;;; FROM/DOWNFROM - DOWNTO/ABOVE
(define-parser arithmetic-down-13-parser
  (consecutive (lambda (var type-spec from to)
                 (make-instance 'for-as-arithmetic-down
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               (alternative from-parser downfrom-parser)
               (alternative downto-parser above-parser)))

;;; DOWNFROM - TO/DOWNTO
(define-parser arithmetic-down-14-parser
  (consecutive (lambda (var type-spec from to)
                 (make-instance 'for-as-arithmetic-down
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :end-form (cdr to)
                   :termination-test (car to)))
               simple-var-parser
               optional-type-spec-parser
               downfrom-parser
               (alternative downto-parser to-parser)))

;;; DOWNFROM - BY
(define-parser arithmetic-down-15-parser
  (consecutive (lambda (var type-spec from by)
                 (make-instance 'for-as-arithmetic-down
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from
                   :by-form by))
               simple-var-parser
               optional-type-spec-parser
               downfrom-parser
               by-parser))

;;; DOWNFROM
(define-parser arithmetic-down-16-parser
  (consecutive (lambda (var type-spec from)
                 (make-instance 'for-as-arithmetic-down
                   :order '(from to by)
                   :var-spec var
                   :type-spec type-spec
                   :start-form from))
               simple-var-parser
               optional-type-spec-parser
               downfrom-parser))

(define-parser three-keyword-down-parser
  (alternative arithmetic-down-1-parser
               arithmetic-down-2-parser
               arithmetic-down-3-parser
               arithmetic-down-4-parser
               arithmetic-down-5-parser
               arithmetic-down-6-parser
               arithmetic-down-7-parser
               arithmetic-down-8-parser
               arithmetic-down-9-parser
               arithmetic-down-10-parser
               arithmetic-down-11-parser
               arithmetic-down-12-parser
               arithmetic-down-13-parser
               arithmetic-down-14-parser
               arithmetic-down-15-parser
               arithmetic-down-16-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define a global parser that tries all the arithmetic parsers until
;;; one succeeds.  We define it so that the parsers that require the
;;; largest number of tokens are tested first.  We must do it that
;;; way, because otherwise, a parser requiring a smaller number of
;;; tokens may succeed without having parsed the following tokens.
;;; Those unparsed tokens will then provoke a parse failure when an
;;; attempt is made to parse them as a clause.

(define-parser for-as-arithmetic-parser
  (alternative three-keyword-up-parser
               three-keyword-down-parser
               two-keyword-up-parser
               one-keyword-up-parser))

(add-for-as-subclause-parser for-as-arithmetic-parser)
(defclass for-as-list (for-as-subclause)
  (list-form
   (list-var (gensym))
   by-form
   (by-var (gensym))
   (rest-var (gensym)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the bindings.

  (initial-bindings (clause)
    `((,(clause 'list-var) ,(clause 'list-form))
      ,@(if (simple-by-form? (clause 'by-form))
            '()
            `((,(clause 'by-var) ,(clause 'by-form))))))

  (final-bindings (clause)
    `((,(clause 'rest-var) ,(clause 'list-var))
      ,@(let ((d-var-spec (clause 'var-spec))
              (d-type-spec (clause 'type-spec)))
          (map (compose (rbind list #<undefined>) car) (extract-variables d-var-spec d-type-spec)))))

 (bound-variables (subclause)
   (map car
        (extract-variables (subclause 'var-spec) #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the declarations.

  (declarations (clause)
    '()) ;todo
  )

(define (simple-by-form? f)
  (or (symbol? f) (member f (list cdr cddr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-IN-LIST.

(defclass for-as-in-list (for-as-list) ()
  (prologue-form (clause end-tag)
    `(begin ,(termination-form clause end-tag)
            ,(generate-assignments (clause 'var-spec) `(car ,(clause 'rest-var)))
            (set! ,(clause 'rest-var)
                  (,(if (simple-by-form? (clause 'by-form)) (clause 'by-form) (clause 'by-var)) ,(clause 'rest-var)))))

  (termination-form (clause end-tag)
    `(when (null? ,(clause 'rest-var))
       (,end-tag)))

  (step-form ((clause for-as-in-list))
    `(begin ,(generate-assignments (clause 'var-spec) `(car ,(clause 'rest-var)))
            (set! ,(clause 'rest-var)
                  (,(if (simple-by-form? (clause 'by-form)) (clause 'by-form) (clause 'by-var)) ,(clause 'rest-var))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-in-list-parser-1
  (consecutive (lambda (var type-spec in list-form by-form)
                 (make-instance 'for-as-in-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form by-form))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'in)
               anything-parser
               by-parser))

(define-parser for-as-in-list-parser-2
  (consecutive (lambda (var type-spec in list-form)
                 (make-instance 'for-as-in-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form cdr))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'in)
               anything-parser))

;;; Define a parser that tries the longer form first
(define-parser for-as-in-list-parser
  (alternative for-as-in-list-parser-1
               for-as-in-list-parser-2))

(add-for-as-subclause-parser for-as-in-list-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ON-LIST.

(defclass for-as-on-list (for-as-list) ()
  (prologue-form (clause end-tag)
    `(begin ,(termination-form clause end-tag)
            ,(generate-assignments (clause 'var-spec) (clause 'rest-var))
            (set! ,(clause 'rest-var)
                  (,(if (simple-by-form? (clause 'by-form)) (clause 'by-form) (clause 'by-var)) ,(clause 'rest-var)))))

  (termination-form (clause end-tag)
    `(unless (pair? ,(clause 'rest-var))
       (,end-tag)))

  (step-form (clause)
    `(begin ,(generate-assignments (clause 'var-spec) (clause 'rest-var))
            (set! ,(clause 'rest-var)
                   (,(if (simple-by-form? (clause 'by-form)) (clause 'by-form) (clause 'by-var)) ,(clause 'rest-var))))))

(define-parser for-as-on-list-parser-1
  (consecutive (lambda (var type-spec on list-form by-form)
                 (make-instance 'for-as-on-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form by-form))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'on)
               anything-parser
               by-parser))

(define-parser for-as-on-list-parser-2
  (consecutive (lambda (var type-spec on list-form)
                 (make-instance 'for-as-on-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form cdr))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'on)
               anything-parser))

;;; Define a parser that tries the longer form first
(define-parser for-as-on-list-parser
  (alternative for-as-on-list-parser-1
               for-as-on-list-parser-2))

(add-for-as-subclause-parser for-as-on-list-parser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-EQUALS-THEN.

(defclass for-as-equals-then (for-as-subclause)
  (initial-form subsequent-form)

 (bound-variables ((subclause for-as-equals-then))
   (map car (extract-variables (subclause 'var-spec) #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the bindings.

  (initial-bindings (clause)
    (let ((d-var-spec (clause 'var-spec))
          (d-type-spec (clause 'type-spec)))
      (map (compose (rbind list #<undefined>) car) (extract-variables d-var-spec d-type-spec))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the declarations.

  (declarations (clause)
    '()) ;todo...?

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the prologue-form.

  (prologue-form (clause end-tag)
    (pidgin-destructuring-bind (temp-tree dictionary)
        (fresh-variables (clause 'var-spec))
      `(let* ,(destructure-variables temp-tree (clause 'initial-form))
         ,@(map (lambda (ot) `(set! ,(car ot) ,(cdr ot))) dictionary))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the step-form.

  (step-form (clause)
    (pidgin-destructuring-bind (temp-tree dictionary)
        (fresh-variables (clause 'var-spec))
      `(let* ,(destructure-variables temp-tree (clause 'subsequent-form))
         ,@(map (lambda (ot) `(set! ,(car ot) ,(cdr ot))) dictionary)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-equals-then-parser-1
  (consecutive (lambda (var-spec type-spec = form1 then form2)
                 (make-instance 'for-as-equals-then
                   :var-spec var-spec
                   :type-spec type-spec
                   :initial-form form1
                   :subsequent-form form2))
               ;; Accept anything for now.  Analyze later.
               anything-parser
               optional-type-spec-parser
               (keyword-parser '=)
               anything-parser
               (keyword-parser 'then)
               anything-parser))

(define-parser for-as-equals-then-parser-2
  (consecutive (lambda (var-spec type-spec = form1)
                 (make-instance 'for-as-equals-then
                   :var-spec var-spec
                   :type-spec type-spec
                   :initial-form form1
                   :subsequent-form form1))
               ;; Accept anything for now.  Analyze later.
               anything-parser
               optional-type-spec-parser
               (keyword-parser '=)
               anything-parser))

;;; Make sure parser 1 is tried first.  For that, it must be added
;;; last.
(add-for-as-subclause-parser for-as-equals-then-parser-2)
(add-for-as-subclause-parser for-as-equals-then-parser-1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ACROSS

(defclass for-as-across (for-as-subclause var-and-type-spec-mixin)
  (;; This slot contains a copy of the tree contained in the VAR-SPEC
   ;; slot except that the non-NIL leaves have been replaced by
   ;; GENSYMs.
   temp-vars
   ;; This slot contains a list of pairs.  Each pair is a CONS cell
   ;; where the CAR is a variable in VAR-SPEC and the CDR is the
   ;; corresponding variable in TEMP-VARS.
   dictionary
   iterator-form
   (form-var (gensym))
   (next-item-var (gensym)))

  ;;; The FOR-AS-ACROSS clasue binds all the variables in the VAR-SPEC
  ;;; of the clause, so this method should return a list of all those
  ;;; variables.
  (bound-variables (clause)
    (map car
         (extract-variables (clause 'var-spec) #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute bindings.

  (initial-bindings (clause)
    `((,(clause 'form-var) (make-iterator ,(clause 'iterator-form)
                                          ; if we're not destructuring, user expects unique pairs
                                          ; but if we are, user never gets at the pairs
                                          ; so we're free to reuse
                                          ,@(if (pair? (clause 'var-spec))
                                              '((cons '() '()))
                                              '())))
      (,(clause 'next-item-var) #<undefined>)))

  (final-bindings (clause)
    `(,@(map (compose (rbind list #<undefined>) car) (clause 'dictionary))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute declarations.

  (declarations (clause)
    '()) ;todo

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute prologue-form.

  (prologue-form (clause end-tag)
    `(begin (set! ,(clause 'next-item-var) (,(clause 'form-var)))
            ,(termination-form clause end-tag)
            ,(generate-assignments (clause 'var-spec)
                                   (clause 'next-item-var))
            (set! ,(clause 'next-item-var) (,(clause 'form-var)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute termination-form

  (termination-form (clause end-tag)
    `(when (iterator-at-end? ,(clause 'form-var))
       (,end-tag)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute step-form.

  (step-form (clause)
    `(begin ,(generate-assignments (clause 'var-spec)
                                   (clause 'next-item-var))
            (set! ,(clause 'next-item-var) (,(clause 'form-var))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across-parser
  (consecutive (lambda (var type-spec across iterator-form)
                 (pidgin-destructuring-bind (temp-vars dictionary)
                     (fresh-variables var)
                   (make-instance 'for-as-across
                     :var-spec var
                     :type-spec type-spec
                     :temp-vars temp-vars
                     :dictionary dictionary
                     :iterator-form iterator-form)))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'across)
               anything-parser))

(add-for-as-subclause-parser for-as-across-parser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntactic and semantic analysis

;;; Check that if there is a name-clause, the last one is in position
;;; zero.
(define (check-name-clause-position clauses)
  (let ((name-clause-position
          (position-if-from-end (rbind type? 'name-clause) clauses)))
    (when (and name-clause-position (positive? name-clause-position))
      (error 'name-clause-not-first))))

;;; Check that there is not a variable-clause following a main clause.
;;; Recall that we diverge from the BNF grammar in the HyperSpec so
;;; that INITIALLY and FINALLY are neither main clauses nor variable
;;; clauses.
(define (check-order-variable-clause-main-clause clauses)
  (let ((last-variable-clause-position
          (position-if-from-end (rbind type? 'variable-clause) clauses))
        (first-main-clause-position
          (position-if (rbind type? 'main-clause) clauses)))
    (when (and last-variable-clause-position
               first-main-clause-position
               (> last-variable-clause-position first-main-clause-position))
      (error 'invalid-clause-order))))

(define (verify-clause-order clauses)
  (check-name-clause-position clauses)
  (check-order-variable-clause-main-clause clauses))

(define (check-variable-uniqueness clauses)
  (let* ((variables (apply append (map bound-variables clauses)))
         (unique-variables (remove-duplicates variables eq?)))
    (unless (= (length variables)
               (length unique-variables))
      (map (lambda (var) (when (> (count var variables eq?) 1)
                           (error 'multiple-variable-occurrences
                                  :bound-variable var)))
           unique-variables))))

;;; Check that for a given accumulation variable, there is only one
;;; category.  Recall that the accumlation categores are represented
;;; by the symbols LIST, COUNT/SUM, and MAX/MIN.
(define (check-accumulation-categories clauses)
  (let* ((descriptors (apply append (map accumulation-variables clauses)))
         (equal-fun (lambda (d1 d2)
                      (and (eq? (car d1) (car d2))
                           (eq? (cadr d1) (cadr d2)))))
         (unique (remove-duplicates descriptors equal-fun)))
    (for-each-on (lambda (remaining)
                   (let ((entry (member (caar remaining)
                                (cdr remaining)
                                (hook eq? car))))
                     (when entry
                       (error "the accumulation variable ~s is used both for ~s accumulation and ~s accumulation."
                              (caar remaining)
                              (cdar remaining)
                              (cdar entry)))))
                 unique)))

;;; Check that there is no overlap between the bound variables and the
;;; accumulation variables.
(define (check-no-variable-overlap clauses)
  (let ((bound-variables
          (apply append (map bound-variables clauses)))
        (accumulation-variables
          (map car
               (apply append
                      (map accumulation-variables clauses)))))
    (let ((intersection
            (intersection bound-variables accumulation-variables
                          eq?)))
      (unless (null? intersection)
        (error "The variable ~s is used both as an iteration variable and as an accumulation variable."
               (car intersection))))))

;;; FIXME: Add more analyses.
(define (analyze-clauses clauses)
  (verify-clause-order clauses)
  (check-variable-uniqueness clauses)
  (check-accumulation-categories clauses)
  (check-no-variable-overlap clauses))
;;; This function is called in a SUM clause in order to sum the
;;; accumulated value with the new one.
(define (sum x . y)
  (for-each (lambda (y)
              (unless (number? y)
                (error 'sum-argument-must-be-number
                       :datum y
                       :expected-type 'number))
              (set! x (+ x y)) y)
            y)
  x)

;;; This function is called in MAX and MIN clauses to ensure that new values
;;; are real.
(define (ensure-real x what)
  (unless (real? x)
    (error what
           :datum x
           :expected-type 'real))
  x)


;;; This function is called in a MAX clause in order to compute the
;;; max of the accumulated value and the new one.
(define (maximize x . y)
  (for-each (lambda (y)
              (set! x (max x (ensure-real y 'max-argument-must-be-real))))
            y)
  x)

;;; This function is called in a MIN clause in order to compute the
;;; min of the accumulated value and the new one.
(define (minimize x . y)
  (for-each (lambda (y)
              (set! x (min x (ensure-real y 'min-argument-must-be-real))))
            y)
  x)

;;; This function is called during restructuring to compute the CAR of
;;; some value.  If that value turns out not to be a LIST, then an
;;; error is signaled.
(define (list-car x)
  (if (pair? x)
      (car x)
      (error 'value-must-be-list
             :datum x
             :expected-type 'list)))

;;; This function is called during restructuring to compute the CDR of
;;; some value.  If that value turns out not to be a LIST, then an
;;; error is signaled.
(define (list-cdr x)
  (if (pair? x)
      (cdr x)
      (error 'value-must-be-list
             :datum x
             :expected-type 'list)))

(define (last x)
  (if (not (pair? (cdr x)))
    x
    (last (cdr x))))

(define (copy-list x)
  (if (not (pair? x)) x (cons (car x) (copy-list (cdr x)))))
(macro forms (expand-body forms))))