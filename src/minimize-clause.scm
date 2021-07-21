(defclass minimize-clause (numeric-accumulation-clause) ((accumulation-category 'min)))

(defclass minimize-it-clause (minimize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
          (,minimize ,*accumulation-variable* ,*it-var*))))

(defclass minimize-form-clause (minimize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
          (apply ,minimize ,*accumulation-variable* (list-values ,(clause 'form))))))

(defclass minimize-it-into-clause (into-mixin minimize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
          (,minimize ,(clause 'into-var) ,*it-var*))))

(defclass minimize-form-into-clause (into-mixin minimize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
          (apply ,minimize ,(clause 'into-var) (list-values ,(clause 'form))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-it-into-clause-parser
  (consecutive (lambda (minimize it into var type-spec)
                 (make-instance 'minimize-it-into-clause
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'minimize)
                            (keyword-parser 'minimizing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity symbol?)
               optional-type-spec-parser))

(define-parser minimize-it-clause-parser
  (consecutive (lambda (minimize it type-spec)
                 (make-instance 'minimize-it-clause
                   :type-spec type-spec))
               (alternative (keyword-parser 'minimize)
                            (keyword-parser 'minimizing))
               (keyword-parser 'it)
               optional-type-spec-parser))

(define-parser minimize-form-into-clause-parser
  (consecutive (lambda (minimize form into var type-spec)
                 (make-instance 'minimize-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'minimize)
                            (keyword-parser 'minimizing))
               anything-parser
               (keyword-parser 'into)
               (singleton identity symbol?)
               optional-type-spec-parser))

(define-parser minimize-form-clause-parser
  (consecutive (lambda (minimize form type-spec)
                 (make-instance 'minimize-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'minimize)
                            (keyword-parser 'minimizing))
               anything-parser
               optional-type-spec-parser))

(define-parser minimize-clause-parser
  (alternative minimize-it-into-clause-parser
               minimize-it-clause-parser
               minimize-form-into-clause-parser
               minimize-form-clause-parser))

(add-clause-parser minimize-clause-parser)
