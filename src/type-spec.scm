;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser simple-type-spec-parser
  (lambda (tokens)
    (if (and (not (null? tokens))
             (member (car tokens) '(integer? let? list? hash-table? float? string? vector? byte-vector? float-vector? int-vector?)))
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
