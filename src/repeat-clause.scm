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
