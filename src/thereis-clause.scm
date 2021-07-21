(defclass thereis-clause (termination-test-clause form-mixin) ()
  (accumulation-variables (clause)
    `((#f thereis t)))

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
