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
