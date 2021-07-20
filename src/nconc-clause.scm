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
                      ,(clause 'form))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set-cdr! ,*list-tail-accumulation-variable*
                        ,(clause 'form))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))))))

(defclass nconc-it-into-clause (into-mixin nconc-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(clause 'into-var)
                      ,*it-var*)
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(clause 'into-var))))
         (begin (set-cdr! ,(tail-variable (clause 'into-var))
                        ,*it-var*)
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))))))

(defclass nconc-form-into-clause (into-mixin nconc-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(clause 'into-var)
                      ,(clause 'form))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(clause 'into-var))))
         (begin (set-cdr! ,(tail-variable (clause 'into-var))
                        ,(clause 'form))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))))))

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
