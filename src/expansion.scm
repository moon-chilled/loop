(define (tail-variable head-variable)
  (let ((result (*tail-variables* head-variable)))
    (unless result
      (set! result (gensym))
      (set! (*tail-variables* head-variable) result))
    result))

(define (accumulation-bindings clauses)
  (let* ((descriptors
           (apply append
                  (map accumulation-variables clauses)))
         (equal-fun (lambda (d1 d2)
                      (and (eq? (car d1) (car d2))
                           (eq? (cadr d1) (cadr d2)))))
         (unique (remove-duplicates descriptors equal-fun)))
    (let loop ((unique unique))
      (if (null? unique)
        '()
        (let ((name (caar unique))
              (category (cadar unique))
              (type (caddar unique)))
          (let ((initial-value (cond ((eq? category 'count/sum) (car (arithmetic-value-and-type type))) ;(coerce 0 type)
                                     ((eq? category 'always/never) #t)
                                     ((eq? category 'max) -inf.0)
                                     ((eq? category 'min) +inf.0)
                                     (#t ''()))))
            (append
              (if (not name)
                `((,*accumulation-variable* ,initial-value))
                `((,name ,initial-value)))
              (if (eq? category 'list)
                (if (not name)
                  `((,*list-tail-accumulation-variable* '()))
                  `((,(tail-variable name) '())))
                '())
              (loop (cdr unique)))))))))

(define *clause* #f)
(define (prologue-body-epilogue clauses end-tag)
  (let ((start-tag (gensym)))
    `(letrec ((,end-tag (lambda ()
                          ,@(map epilogue-form clauses)
                          (,*loop-return-sym*
                            ,*accumulation-variable*))))
       (letrec ((,start-tag (lambda ()
                              ,@(map (lambda (clause)
                                       (body-form clause end-tag))
                                     clauses)
                              ,@(map (lambda (clause)
                                       (termination-form clause end-tag))
                                     clauses)
                              ,@(map step-form clauses)
                              (,start-tag))))
                ,@(map (lambda (clause)
                         (prologue-form clause end-tag)) clauses)
                (,start-tag)))))

;;; Process all clauses by first computing the prologue, the body, and
;;; the epilogue, and then applying the clause-specific wrapper for
;;; each clause to the result.
(define (do-clauses all-clauses end-tag)
  (let ((result (prologue-body-epilogue all-clauses end-tag)))
    (map (lambda (clause)
           (set! result (wrap-clause clause result)))
         (reverse all-clauses))
    result))

(define (expand-clauses all-clauses end-tag)
  (let ((acc (accumulation-bindings all-clauses)))
    `(let (,@(if (member *accumulation-variable* (map car acc))
                 '()
                 `((,*accumulation-variable* '()))) ;*accumulation-variable* was nil originally; is '() right?
           ,@acc)
       ,(do-clauses all-clauses end-tag))))

; consider making a dedicated 'loop error' tag and stuffing the actual data
; into the tag so that we don't have to awkwardly rethrow other errors?
(define-macro (augment-error-with-nice-message . body)
  `(catch #t (lambda () ,@body)
          (lambda (err rest)
            (if (,*loop-errors* err)
              (error err (apply (,*loop-errors* err) #f rest))
              (apply error err rest)))))

(define (expand-body loop-body)
  (augment-error-with-nice-message
    (if (every pair? loop-body)
        (let ((tag (gensym)))
          `(call-with-exit
             (letrec ((,tag (lambda (return)
                              ,@loop-body
                              (,tag return))))
               ,tag)))
        (let ((clauses (parse-loop-body loop-body))
              (end-tag (gensym)))
          (analyze-clauses clauses)
          (let-temporarily ((*loop-name* (if (type? (car clauses) 'name-clause)
                                           ((car clauses) 'name)
                                           #f))
                            (*loop-return-sym* (gensym))
                            (*accumulation-variable* (gensym))
                            (*list-tail-accumulation-variable* (gensym))
                            (*tail-variables* (make-hash-table 8 eq?)))
            ; todo incorporate *loop-name* to allow named return
            `(,augment-error-with-nice-message
               (call-with-exit
                 (lambda (return)
                   (call-with-exit
                     (lambda (,*loop-return-sym*)
                       (let ((loop-finish (macro () `(,',end-tag))))
                         ,(expand-clauses clauses end-tag))))))))))))
