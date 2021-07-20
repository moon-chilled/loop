;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ACROSS

(defclass for-as-across (for-as-subclause var-and-type-spec-mixin)
  (;; This slot contains a copy of the tree contained in the VAR-SPEC
   ;; slot except that the non-NIL leaves have been replaced by
   ;; GENSYMs.
   temp-vars
   ;; This slot contains a list of pairs.  Each pair is a CONS cell
   ;; where the CAR is a variable in VAR-SPEC and the CDR is the
   ;; corresponding variable in TEMP-VARS.
   dictionary
   iterator-form
   (form-var (gensym))
   (next-item-var (gensym)))

  ;;; The FOR-AS-ACROSS clasue binds all the variables in the VAR-SPEC
  ;;; of the clause, so this method should return a list of all those
  ;;; variables.
  (bound-variables (clause)
    (map car
         (extract-variables (clause 'var-spec) #f)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute bindings.
  
  (initial-bindings (clause)
    `((,(clause 'form-var) (make-iterator ,(clause 'iterator-form)
                                          ; if we're not destructuring, user expects unique pairs
                                          ; but if we are, user never gets at the pairs
                                          ; so we're free to reuse
                                          ,@(if (pair? (clause 'var-spec))
                                              '((cons '() '()))
                                              '())))
      (,(clause 'next-item-var) #<undefined>)))
  
  (final-bindings (clause)
    `(,@(map (compose (rbind list #<undefined>) car) (clause 'dictionary))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute declarations.
  
  (declarations (clause)
    '()) ;todo

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute prologue-form.
  
  (prologue-form (clause end-tag)
    `(begin (set! ,(clause 'next-item-var) (,(clause 'form-var)))
            ,(termination-form clause end-tag)
            ,(generate-assignments (clause 'var-spec)
                                   (clause 'next-item-var))
            (set! ,(clause 'next-item-var) (,(clause 'form-var)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute termination-form
  
  (termination-form (clause end-tag)
    `(when (iterator-at-end? ,(clause 'form-var))
       (,end-tag)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute step-form.
  
  (step-form (clause)
    `(begin ,(generate-assignments (clause 'var-spec)
                                   (clause 'next-item-var))
            (set! ,(clause 'next-item-var) (,(clause 'form-var))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across-parser
  (consecutive (lambda (var type-spec across iterator-form)
                 (pidgin-destructuring-bind (temp-vars dictionary)
                     (fresh-variables var)
                   (make-instance 'for-as-across
                     :var-spec var
                     :type-spec type-spec
                     :temp-vars temp-vars
                     :dictionary dictionary
                     :iterator-form iterator-form)))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'across)
               anything-parser))

(add-for-as-subclause-parser for-as-across-parser)
