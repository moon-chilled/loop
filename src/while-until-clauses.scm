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
