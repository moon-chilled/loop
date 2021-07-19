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
  (optional #t type-spec-parser))
