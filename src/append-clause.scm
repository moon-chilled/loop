(defclass append-clause (list-accumulation-clause) ())

(defclass append-it-clause (append-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*accumulation-variable*
                      (,copy-list ,*it-var*))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set-cdr! ,*list-tail-accumulation-variable*
                        (,copy-list ,*it-var*))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))))))

(defclass append-form-clause (append-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,*list-tail-accumulation-variable*)
         (begin (set! ,*accumulation-variable*
                      (,copy-list ,(clause 'form)))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*accumulation-variable*)))
         (begin (set-cdr! ,*list-tail-accumulation-variable*
                        (,copy-list ,(clause 'form)))
                (set! ,*list-tail-accumulation-variable*
                      (,last ,*list-tail-accumulation-variable*))))))

(defclass append-it-into-clause (into-mixin append-clause it-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(clause 'into-var)
                      (,copy-list ,*it-var*))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(clause 'into-var))))
         (begin (set-cdr! ,(tail-variable (clause 'into-var))
                        (,copy-list ,*it-var*))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))))))

(defclass append-form-into-clause (into-mixin append-clause form-mixin) ()
  (body-form (clause end-tag)
    `(if (null? ,(tail-variable (clause 'into-var)))
         (begin (set! ,(clause 'into-var)
                      (,copy-list ,(clause 'form)))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(clause 'into-var))))
         (begin (set-cdr! ,(tail-variable (clause 'into-var))
                        (,copy-list ,(clause 'form)))
                (set! ,(tail-variable (clause 'into-var))
                      (,last ,(tail-variable (clause 'into-var))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser append-it-into-clause-parser
  (consecutive (lambda (append it into var)
                 (make-instance 'append-it-into-clause
                   :into-var var))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton identity symbol?)))

(define-parser append-it-clause-parser
  (consecutive (lambda (append it)
                 (make-instance 'append-it-clause))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               (keyword-parser 'it)))

(define-parser append-form-into-clause-parser
  (consecutive (lambda (append form into var)
                 (make-instance 'append-form-into-clause
                   :form form
                   :into-var var))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               anything-parser
               (keyword-parser 'into)
               (singleton identity symbol?)))

(define-parser append-form-clause-parser
  (consecutive (lambda (append form)
                 (make-instance 'append-form-clause
                   :form form))
               (alternative (keyword-parser 'append)
                            (keyword-parser 'appending))
               anything-parser))

(define-parser append-clause-parser
  (alternative append-it-into-clause-parser
               append-it-clause-parser
               append-form-into-clause-parser
               append-form-clause-parser))

(add-clause-parser append-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.




