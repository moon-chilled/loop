(defclass sum-clause (count/sum-accumulation-clause) ())

(defclass sum-it-clause (sum-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
           (sum ,*accumulation-variable* ,*it-var*))))

(defclass sum-form-clause (sum-clause form-mixin) ()
  (body-form (clause end-tag)
      `(set! ,*accumulation-variable*
         (sum ,*accumulation-variable* ,(clause 'form)))))

(defclass sum-it-into-clause (into-mixin sum-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (sum ,(clause 'into-var) ,*it-var*))))

(defclass sum-form-into-clause (into-mixin sum-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (sum ,(clause 'into-var) ,(clause 'form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser sum-it-into-clause-parser
  (consecutive (lambda (sum it into var type-spec)
                 (make-instance 'sum-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity
                          symbol?)
               optional-type-spec-parser))

(define-parser sum-it-clause-parser
  (consecutive (lambda (sum it type-spec)
                 (make-instance 'sum-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               (keyword-parser 'it)
               optional-type-spec-parser))

(define-parser sum-form-into-clause-parser
  (consecutive (lambda (sum form into var type-spec)
                 (make-instance 'sum-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               anything-parser
               (keyword-parser 'into)
               (singleton identity
                          symbol?)
               optional-type-spec-parser))

(define-parser sum-form-clause-parser
  (consecutive (lambda (sum form type-spec)
                 (make-instance 'sum-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               anything-parser
               optional-type-spec-parser))

(define-parser sum-clause-parser
  (alternative sum-it-into-clause-parser
               sum-it-clause-parser
               sum-form-into-clause-parser
               sum-form-clause-parser))

(add-clause-parser sum-clause-parser)
