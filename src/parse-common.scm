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
