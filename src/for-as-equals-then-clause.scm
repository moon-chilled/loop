;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-EQUALS-THEN.

(defclass for-as-equals-then (for-as-subclause)
  (initial-form subsequent-form)

 (bound-variables ((subclause for-as-equals-then))
   (map car (extract-variables (subclause 'var-spec) #f)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the bindings.

  (initial-bindings (clause)
    (let ((d-var-spec (clause 'var-spec))
          (d-type-spec (clause 'type-spec)))
      (map (compose (rbind list #<undefined>) car) (extract-variables d-var-spec d-type-spec))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the declarations.

  (declarations (clause)
    '()) ;todo...?

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the prologue-form.

  (prologue-form (clause end-tag)
    (pidgin-destructuring-bind (temp-tree dictionary)
        (fresh-variables (clause 'var-spec))
      `(let* ,(destructure-variables temp-tree (clause 'initial-form))
         ,@(map (lambda (ot) `(set! ,(car ot) ,(cdr ot))) dictionary))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute the step-form.

  (step-form (clause)
    (pidgin-destructuring-bind (temp-tree dictionary)
        (fresh-variables (clause 'var-spec))
      `(let* ,(destructure-variables temp-tree (clause 'subsequent-form))
         ,@(map (lambda (ot) `(set! ,(car ot) ,(cdr ot))) dictionary)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-equals-then-parser-1
  (consecutive (lambda (var-spec type-spec = form1 then form2)
                 (make-instance 'for-as-equals-then
                   :var-spec var-spec
                   :type-spec type-spec
                   :initial-form form1
                   :subsequent-form form2))
               ;; Accept anything for now.  Analyze later.
               anything-parser
               optional-type-spec-parser
               (keyword-parser '=)
               anything-parser
               (keyword-parser 'then)
               anything-parser))

(define-parser for-as-equals-then-parser-2
  (consecutive (lambda (var-spec type-spec = form1)
                 (make-instance 'for-as-equals-then
                   :var-spec var-spec
                   :type-spec type-spec
                   :initial-form form1
                   :subsequent-form form1))
               ;; Accept anything for now.  Analyze later.
               anything-parser
               optional-type-spec-parser
               (keyword-parser '=)
               anything-parser))

;;; Make sure parser 1 is tried first.  For that, it must be added
;;; last.
(add-for-as-subclause-parser for-as-equals-then-parser-2)
(add-for-as-subclause-parser for-as-equals-then-parser-1)
