(defclass count-clause (count/sum-accumulation-clause) ())

(defclass count-it-clause (count-clause it-mixin) ()
  (body-form (clause end-tag)
    `(when ,*it-var*
       (set! ,*accumulation-variable*
             (+ 1 ,*accumulation-variable*)))))

(defclass count-form-clause (count-clause form-mixin) ()
  (body-form (clause end-tag)
    `(when ,(clause 'form)
       (set! ,*accumulation-variable*
             (+ 1 ,*accumulation-variable*)))))

(defclass count-it-into-clause (into-mixin count-clause it-mixin) ()
  (body-form (clause end-tag)
    `(when ,*it-var*
       (set! ,(clause 'into-var)
             (+ 1 ,(clause 'into-var))))))

(defclass count-form-into-clause (into-mixin count-clause form-mixin) ()
  (body-form (clause end-tag)
    `(when ,(clause 'form)
       (set! ,(clause 'into-var)
             (+ 1 ,(clause 'into-var))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser count-it-into-clause-parser
  (consecutive (lambda (count it into var type-spec)
                 (make-instance 'count-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity
                          symbol?)
               optional-type-spec-parser))

(define-parser count-it-clause-parser
  (consecutive (lambda (count it type-spec)
                 (make-instance 'count-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               (keyword-parser 'it)
               optional-type-spec-parser))

(define-parser count-form-into-clause-parser
  (consecutive (lambda (count form into var type-spec)
                 (make-instance 'count-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               anything-parser
               (keyword-parser 'into)
               (singleton identity
                          symbol?)
               optional-type-spec-parser))

(define-parser count-form-clause-parser
  (consecutive (lambda (count form type-spec)
                 (make-instance 'count-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'count)
                            (keyword-parser 'counting))
               anything-parser
               optional-type-spec-parser))

(define-parser count-clause-parser
  (alternative count-it-into-clause-parser
               count-it-clause-parser
               count-form-into-clause-parser
               count-form-clause-parser))

(add-clause-parser count-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.
