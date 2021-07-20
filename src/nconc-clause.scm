(defclass nconc-clause (list-accumulation-clause) ())

(defclass nconc-it-clause (nconc-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*accumulation-variable*
                      ,*it-var*)
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set-cdr! ,*list-tail-accumulation-variable*
                        ,*it-var*)
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))))))

(defclass nconc-form-clause (nconc-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*accumulation-variable*
                      ,(form clause))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set-cdr! ,*list-tail-accumulation-variable*
                        ,(form clause))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))))))

(defclass nconc-it-into-clause (into-mixin nconc-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (into-var clause)))
         (begin (set! ,(into-var clause)
                      ,*it-var*)
                (set! ,(tail-variable (into-var clause))
                      (,last ,(into-var clause))))
         (begin (set-cdr! ,(tail-variable (into-var clause))
                        ,*it-var*)
                (set! ,(tail-variable (into-var clause))
                      (,last ,(tail-variable (into-var clause))))))))

(defclass nconc-form-into-clause (into-mixin nconc-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (into-var clause)))
         (begin (set! ,(into-var clause)
                      ,(form clause))
                (set! ,(tail-variable (into-var clause))
                      (,last ,(into-var clause))))
         (begin (set-cdr! ,(tail-variable (into-var clause))
                        ,(form clause))
                (set! ,(tail-variable (into-var clause))
                      (,last ,(tail-variable (into-var clause))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser nconc-it-into-clause-parser
  (consecutive (lambda (nconc it into var)
                 (make-instance 'nconc-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity symbol?)))

(define-parser nconc-it-clause-parser
  (consecutive (lambda (nconc it)
                 (make-instance 'nconc-it-clause))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               (keyword-parser 'it)))

(define-parser nconc-form-into-clause-parser
  (consecutive (lambda (nconc form into var)
                 (make-instance 'nconc-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               anything-parser
               (keyword-parser 'into)
               (singleton identity symbol?)))

(define-parser nconc-form-clause-parser
  (consecutive (lambda (nconc form)
                 (make-instance 'nconc-form-clause
                   :form form))
               (alternative (keyword-parser 'nconc)
                            (keyword-parser 'nconcing))
               anything-parser))

(define-parser nconc-clause-parser
  (alternative nconc-it-into-clause-parser
               nconc-it-clause-parser
               nconc-form-into-clause-parser
               nconc-form-clause-parser))

(add-clause-parser nconc-clause-parser)
