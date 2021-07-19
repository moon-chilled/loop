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
