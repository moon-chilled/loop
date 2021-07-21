(defclass maximize-clause (numeric-accumulation-clause) ((accumulation-category 'max)))

(defclass maximize-it-clause (maximize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
           (,maximize ,*accumulation-variable* ,*it-var*))))

(defclass maximize-form-clause (maximize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
           (apply ,maximize ,*accumulation-variable* (list-values ,(clause 'form))))))

(defclass maximize-it-into-clause (into-mixin maximize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (,maximize ,(clause 'into-var) ,*it-var*))))

(defclass maximize-form-into-clause (into-mixin maximize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (apply ,maximize ,(clause 'into-var) (list-values ,(clause 'form))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser maximize-it-into-clause-parser
  (consecutive (lambda (maximize it into var type-spec)
                 (make-instance 'maximize-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity symbol?)
               optional-type-spec-parser))

(define-parser maximize-it-clause-parser
  (consecutive (lambda (maximize it type-spec)
                 (make-instance 'maximize-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               (keyword-parser 'it)
               optional-type-spec-parser))

(define-parser maximize-form-into-clause-parser
  (consecutive (lambda (maximize form into var type-spec)
                 (make-instance 'maximize-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               anything-parser
               (keyword-parser 'into)
               (singleton identity symbol?)
               optional-type-spec-parser))

(define-parser maximize-form-clause-parser
  (consecutive (lambda (maximize form type-spec)
                 (make-instance 'maximize-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'maximize)
                            (keyword-parser 'maximizing))
               anything-parser
               optional-type-spec-parser))

(define-parser maximize-clause-parser
  (alternative maximize-it-into-clause-parser
               maximize-it-clause-parser
               maximize-form-into-clause-parser
               maximize-form-clause-parser))

(add-clause-parser maximize-clause-parser)
