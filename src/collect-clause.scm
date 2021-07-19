(defclass collect-clause (list-accumulation-clause) ())

(defclass collect-it-clause (collect-clause it-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*list-tail-accumulation-variable*
                      (list ,*it-var*))
                (set! ,*accumulation-variable*
                      ,*list-tail-accumulation-variable*))
         (begin (set-cdr! ,*list-tail-accumulation-variable*
                          (list ,*it-var*))
                (set! ,*list-tail-accumulation-variable*
                      (cdr ,*list-tail-accumulation-variable*)))))) ;ditto

(defclass collect-form-clause (collect-clause form-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
       (begin (set! ,*list-tail-accumulation-variable*
                    (list ,(clause 'form)))
              (set! ,*accumulation-variable*
                    ,*list-tail-accumulation-variable*))
       (begin (set-cdr! ,*list-tail-accumulation-variable*
                        (list ,(clause 'form)))
              (set! ,*list-tail-accumulation-variable*
                    (cdr ,*list-tail-accumulation-variable*)))))) ;todo should be last-cdr

(defclass collect-it-into-clause (into-mixin collect-clause it-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (into-var clause)))
         (begin (set! ,(tail-variable (into-var clause))
                      (list ,*it-var*))
                (set! ,(into-var clause)
                      ,(tail-variable (into-var clause))))
         (begin (set-cdr! ,(tail-variable (into-var clause))
                          (list ,*it-var*))
                (set! ,(tail-variable (into-var clause))
                      (cdr ,(tail-variable (into-var clause)))))))) ;ditto

(defclass collect-form-into-clause (into-mixin collect-clause form-mixin)
  ()

  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (into-var clause)))
       (begin (set! ,(tail-variable (into-var clause))
                    (list ,(clause 'form)))
              (set! ,(into-var clause)
                    ,(tail-variable (into-var clause))))
       (begin (set-cdr! ,(tail-variable (into-var clause))
                        (list ,(clause 'form)))
              (set! ,(tail-variable (into-var clause))
                    (cdr ,(tail-variable (into-var clause)))))))) ;ditto

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
