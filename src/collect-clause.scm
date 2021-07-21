(defclass collect-clause (list-accumulation-clause) ())

(defclass collect-it-clause (collect-clause it-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*list-tail-accumulation-variable*
                      (list ,*it-var*))
                (set! ,*accumulation-variable*
                      ,*list-tail-accumulation-variable*))
         (begin (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))
                (set-cdr! ,*list-tail-accumulation-variable*
                          (list ,*it-var*))))))

(defclass collect-form-clause (collect-clause form-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
       (begin (set! ,*list-tail-accumulation-variable*
                    (list-values ,(clause 'form)))
              (set! ,*accumulation-variable*
                    ,*list-tail-accumulation-variable*))
       (begin (set! ,*list-tail-accumulation-variable*
                    (,last ,*list-tail-accumulation-variable*))
              (set-cdr! ,*list-tail-accumulation-variable*
                        (list-values ,(clause 'form)))))))

(defclass collect-it-into-clause (into-mixin collect-clause it-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(tail-variable (clause 'into-var))
                      (list ,*it-var*))
                (set! ,(clause 'into-var)
                      ,(tail-variable (clause 'into-var))))
         (begin (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))
                (set-cdr! ,(tail-variable (clause 'into-var))
                          (list ,*it-var*))))))

(defclass collect-form-into-clause (into-mixin collect-clause form-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
       (begin (set! ,(tail-variable (clause 'into-var))
                    (list-values ,(clause 'form)))
              (set! ,(clause 'into-var)
                    ,(tail-variable (clause 'into-var))))
       (begin (set! ,(tail-variable (clause 'into-var))
                    (,last ,(tail-variable (clause 'into-var))))
              (set-cdr! ,(tail-variable (clause 'into-var))
                        (list-values ,(clause 'form)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser collect-it-into-clause-parser
  (consecutive (lambda (collect it into var)
                 (make-instance 'collect-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'collect)
                            (keyword-parser 'collecting))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity
                          symbol?)))

(define-parser collect-it-clause-parser
  (consecutive (lambda (collect it)
                 (make-instance 'collect-it-clause))
               (alternative (keyword-parser 'collect)
                            (keyword-parser 'collecting))
               (keyword-parser 'it)))

(define-parser collect-form-into-clause-parser
  (consecutive (lambda (collect form into var)
                 (make-instance 'collect-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'collect)
                            (keyword-parser 'collecting))
               anything-parser
               (keyword-parser 'into)
               (singleton identity
                          symbol?)))

(define-parser collect-form-clause-parser
  (consecutive (lambda (collect form)
                 (make-instance 'collect-form-clause
                   :form form))
               (alternative (keyword-parser 'collect)
                            (keyword-parser 'collecting))
               anything-parser))

(define-parser collect-clause-parser
  (alternative collect-it-into-clause-parser
               collect-it-clause-parser
               collect-form-into-clause-parser
               collect-form-clause-parser))

(add-clause-parser collect-clause-parser)
