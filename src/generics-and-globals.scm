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
