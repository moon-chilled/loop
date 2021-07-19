(define transform-tagbody
  (let ()
    ;; from guile-user I think
    ;; (block LABEL FORMS...)
    ;;
    ;; Execute FORMS.  Within FORMS, a lexical binding named LABEL is
    ;; visible that contains an escape function for the block.  Calling
    ;; the function in LABEL with a single argument will immediatly stop
    ;; the execution of FORMS and return the argument as the value of the
    ;; block.  If the function in LABEL is not invoked, the value of the
    ;; block is the value of the last form in FORMS.

    (define-macro (block label . forms)
                  `(let ((body (lambda (,label) ,@forms))
                         (tag (gensym "return-")))
                     (catch tag
                            (lambda () (body (lambda (val) (throw tag val))))
                            (lambda (tag val) val))))

    ;; (with-return FORMS...)
    ;;
    ;; Equivalent to (block return FORMS...)

    (define-macro (with-return . forms)
                  `(block return ,@forms))

    ;; (tagbody TAGS-AND-FORMS...)
    ;;
    ;; TAGS-AND-FORMS is a list of either tags or forms.  A TAG is a
    ;; symbol while a FORM is everything else.  Normally, the FORMS are
    ;; executed sequentially.  However, control can be transferred to the
    ;; forms following a TAG by invoking the tag as a function.  That is,
    ;; within the FORMS, there is a lexical binding for each TAG with the
    ;; symbol that is the tag as its name.  The bindings carry functions
    ;; that will execute the FORMS following the respective TAG.
    ;;
    ;; The value of a tagbody is always `#f'.

    (define (transform-tagbody forms)
      (let ((start-tag (gensym "start-"))
            (block-tag (gensym "block-")))
        (let loop ((cur-tag start-tag)
                   (cur-code ())
                   (tags-and-code ())
                   (forms forms))
          (cond
            ((null? forms)
             `(,block ,block-tag
                     (letrec ,(reverse! (cons (list cur-tag `(lambda () ,@(reverse! (cons `(,block-tag #f) cur-code)))) tags-and-code))
                       (,start-tag))))
            ((symbol? (car forms))
             (loop (car forms)
                   '()
                   (cons (list cur-tag `(lambda () ,@(reverse! (cons `(,(car forms)) cur-code)))) tags-and-code)
                   (cdr forms)))
            (else
              (loop cur-tag
                    (cons (car forms) cur-code)
                    tags-and-code
                    (cdr forms)))))))
    transform-tagbody))
;(define-macro (transform-tagbody . forms) (transform-tagbody forms))
