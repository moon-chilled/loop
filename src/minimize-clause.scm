(defclass minimize-clause (max/min-accumulation-clause) ())

(defclass minimize-it-clause (minimize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*accumulation-variable*)
         (set! ,*accumulation-variable* (ensure-real ,*it-var* 'min-argument-must-be-real))
         (set! ,*accumulation-variable*
               (,minimize ,*accumulation-variable* ,*it-var*)))))

(defclass minimize-form-clause (minimize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*accumulation-variable*)
         (set! ,*accumulation-variable* (ensure-real ,(clause 'form) 'min-argument-must-be-real))
         (set! ,*accumulation-variable*
               (,minimize ,*accumulation-variable* ,(clause 'form))))))

(defclass minimize-it-into-clause (into-mixin minimize-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(clause 'into-var))
         (set! ,(clause 'into-var) (ensure-real ,*it-var* 'min-argument-must-be-real))
         (set! ,(clause 'into-var)
               (,minimize ,(clause 'into-var) ,*it-var*)))))

(defclass minimize-form-into-clause (into-mixin minimize-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(clause 'into-var))
         (set! ,(clause 'into-var) (ensure-real ,(clause 'form) 'min-argument-must-be-real))
         (set! ,(clause 'into-var)
               (,minimize ,(clause 'into-var) ,(clause 'form))))))


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
