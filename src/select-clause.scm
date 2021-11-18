(defclass select-clause (string-accumulation-clause) ())

(defclass select-it-clause (select-clause it-mixin)
  ()

  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
           (,string-together ,*accumulation-variable*
                             ,*it-var*))))

(defclass select-form-clause (select-clause form-mixin)
  ()

  (body-form (clause end-tag)
    `(set! ,*accumulation-variable*
       (apply ,string-together ,*accumulation-variable* (list-values ,(clause 'form))))))

(defclass select-it-into-clause (into-mixin select-clause it-mixin)
  ()

  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (,string-together ,(clause 'into-var)
                             ,*it-var*))))

(defclass select-form-into-clause (into-mixin select-clause form-mixin)
  ()

  (body-form (clause end-tag)
    `(set! ,(clause 'into-var)
           (apply ,string-together ,(clause 'into-var) (list-values ,(clause 'form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser select-it-into-clause-parser
  (consecutive (lambda (select it into var)
                 (make-instance 'select-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'select)
                            (keyword-parser 'selecting))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity
                          symbol?)))

(define-parser select-it-clause-parser
  (consecutive (lambda (select it)
                 (make-instance 'select-it-clause))
               (alternative (keyword-parser 'select)
                            (keyword-parser 'selecting))
               (keyword-parser 'it)))

(define-parser select-form-into-clause-parser
  (consecutive (lambda (select form into var)
                 (make-instance 'select-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'select)
                            (keyword-parser 'selecting))
               anything-parser
               (keyword-parser 'into)
               (singleton identity
                          symbol?)))

(define-parser select-form-clause-parser
  (consecutive (lambda (select form)
                 (make-instance 'select-form-clause
                   :form form))
               (alternative (keyword-parser 'select)
                            (keyword-parser 'selecting))
               anything-parser))

(define-parser select-clause-parser
  (alternative select-it-into-clause-parser
               select-it-clause-parser
               select-form-into-clause-parser
               select-form-clause-parser))

(add-clause-parser select-clause-parser)
