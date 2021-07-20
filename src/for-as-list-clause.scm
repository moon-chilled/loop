(defclass for-as-list (for-as-subclause)
  (list-form
   (list-var (gensym))
   by-form
   (by-var (gensym))
   (rest-var (gensym)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the bindings.
  
  (initial-bindings (clause)
    `((,(clause 'list-var) ,(clause 'list-form))
      ,@(if (simple-by-form? (clause 'by-form))
            '()
            `((,(clause 'by-var) ,(clause 'by-form))))))
  
  (final-bindings (clause)
    `((,(clause 'rest-var) ,(clause 'list-var))
      ,@(let ((d-var-spec (clause 'var-spec))
              (d-type-spec (clause 'type-spec)))
          (map (compose (rbind list #<undefined>) car) (extract-variables d-var-spec d-type-spec)))))

 (bound-variables (subclause)
   (map car
        (extract-variables (subclause 'var-spec) #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the declarations.
  
  (declarations (clause)
    '()) ;todo
  )

(define (simple-by-form? f)
  (or (symbol? f) (member f (list cdr cddr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-IN-LIST.

(defclass for-as-in-list (for-as-list) ()
  (prologue-form (clause end-tag)
    `(begin ,(termination-form clause end-tag)
            ,(generate-assignments (clause 'var-spec) `(car ,(clause 'rest-var)))
            (set! ,(clause 'rest-var)
                  (,(if (simple-by-form? (clause 'by-form)) (clause 'by-form) (clause 'by-var)) ,(clause 'rest-var)))))

  (termination-form (clause end-tag)
    `(when (null? ,(clause 'rest-var))
       (,end-tag)))
  
  (step-form ((clause for-as-in-list))
    `(begin ,(generate-assignments (clause 'var-spec) `(car ,(clause 'rest-var)))
            (set! ,(clause 'rest-var)
                  (,(if (simple-by-form? (clause 'by-form)) (clause 'by-form) (clause 'by-var)) ,(clause 'rest-var))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-in-list-parser-1
  (consecutive (lambda (var type-spec in list-form by-form)
                 (make-instance 'for-as-in-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form by-form))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'in)
               anything-parser
               by-parser))

(define-parser for-as-in-list-parser-2
  (consecutive (lambda (var type-spec in list-form)
                 (make-instance 'for-as-in-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form cdr))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'in)
               anything-parser))

;;; Define a parser that tries the longer form first
(define-parser for-as-in-list-parser
  (alternative for-as-in-list-parser-1
               for-as-in-list-parser-2))

(add-for-as-subclause-parser for-as-in-list-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ON-LIST.

(defclass for-as-on-list (for-as-list) ()
  (prologue-form (clause end-tag)
    `(begin ,(termination-form clause end-tag)
            ,(generate-assignments (clause 'var-spec) (clause 'rest-var))
            (set! ,(clause 'rest-var)
                  (,(if (simple-by-form? (clause 'by-form)) (clause 'by-form) (clause 'by-var)) ,(clause 'rest-var)))))
  
  (termination-form (clause end-tag)
    `(unless (pair? ,(clause 'rest-var))
       (,end-tag)))

  (step-form (clause)
    `(begin ,(generate-assignments (clause 'var-spec) (clause 'rest-var))
            (set! ,(clause 'rest-var)
                   (,(if (simple-by-form? (clause 'by-form)) (clause 'by-form) (clause 'by-var)) ,(clause 'rest-var))))))

(define-parser for-as-on-list-parser-1
  (consecutive (lambda (var type-spec on list-form by-form)
                 (make-instance 'for-as-on-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form by-form))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'on)
               anything-parser
               by-parser))

(define-parser for-as-on-list-parser-2
  (consecutive (lambda (var type-spec on list-form)
                 (make-instance 'for-as-on-list
                   :var-spec var
                   :type-spec type-spec
                   :list-form list-form
                   :by-form cdr))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'on)
               anything-parser))

;;; Define a parser that tries the longer form first
(define-parser for-as-on-list-parser
  (alternative for-as-on-list-parser-1
               for-as-on-list-parser-2))

(add-for-as-subclause-parser for-as-on-list-parser)
