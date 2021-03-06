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
