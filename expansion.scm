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
          (let ((initial-value (cond ((eq? category 'count/sum) 0) ;(coerce 0 type)
                                     ((eq? category 'always/never) #t)
                                     (#t '()))))
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

(define (prologue-body-epilogue clauses end-tag)
  (let ((start-tag (gensym)))
    (transform-tagbody
        `((begin ,@(map (lambda (clause)
                          (format #t "exp~%")
                             (prologue-form clause end-tag))
                           clauses))
          ,start-tag
          (begin ,@(map (lambda (clause)
                             (body-form clause end-tag))
                           clauses))
          (begin ,@(map (lambda (clause)
                             (termination-form clause end-tag))
                           clauses))
          (begin ,@(map step-form clauses))
          (,start-tag)
          ,end-tag
          (begin ,@(map epilogue-form clauses)
                 (*loop-return-sym*
                   ,*accumulation-variable*))))))

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
                 `((,*accumulation-variable* nil)))
           ,@acc)
       ,(do-clauses all-clauses end-tag))))

(define (expand-body loop-body end-tag)
  (if (every pair? loop-body)
      (let ((tag (gensym)))
        `(call-with-exit
           (lambda (return)
           ,(transform-tagbody
              `(,tag
                ,@loop-body
                (,tag))))))
      (let ((clauses (parse-loop-body loop-body)))
        (analyze-clauses clauses)
        (let-temporarily ((*loop-name* (if (eq? 'name-clause ((car clauses) 'class-name))
                                         ((car clauses) 'name)
                                         #f))
                          (*loop-return-sym* (gensym))
                          (*accumulation-variable* (gensym))
                          (*list-tail-accumulation-variable* (gensym))
                          (*tail-variables* (make-hash-table 8 eq?)))
          ; todo incorporate *loop-name* to allow named return
          `(call-with-exit
             (lambda (return)
               (call-with-exit
                 (lambda (,*loop-return-sym*)
                   ,@(expand-clauses clauses end-tag)))))))))

(define (analyze-clauses clauses) ()) ;todo analysis.scm
