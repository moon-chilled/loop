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
   vector-form
   (form-var (gensym))
   (length-var (gensym))
   (index-var (gensym)))

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
    `((,(clause 'form-var) ,(clause 'vector-form))
      (,(clause 'index-var) 0)))
  
  (final-bindings (clause)
    `((,(clause 'length-var) (length ,(clause 'form-var)))
      ,@(map (compose (rbind list #<undefined>) car) (clause 'dictionary))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute declarations.
  
  (declarations (clause)
    '()) ;todo

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute prologue-form.
  
  (prologue-form (clause end-tag)
    `(begin ,(termination-form clause end-tag)
            ,(generate-assignments (clause 'var-spec)
                                   `(,(clause 'form-var)
                                     ,(clause 'index-var)))
            (set! ,(clause 'index-var) (+ 1 ,(clause 'index-var)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute termination-form
  
  (termination-form (clause end-tag)
    `(when (>= ,(clause 'index-var) ,(clause 'length-var))
       (,end-tag)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute step-form.
  
  (step-form (clause)
    `(begin ,(generate-assignments (clause 'var-spec)
                                   `(,(clause 'form-var)
                                     ,(clause 'index-var)))
            (set! ,(clause 'index-var) (+ 1 ,(clause 'index-var))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across-parser
  (consecutive (lambda (var type-spec across vector-form)
                 (pidgin-destructuring-bind (temp-vars dictionary)
                     (fresh-variables var)
                   (make-instance 'for-as-across
                     :var-spec var
                     :type-spec type-spec
                     :temp-vars temp-vars
                     :dictionary dictionary
                     :vector-form vector-form)))
               anything-parser
               optional-type-spec-parser
               (keyword-parser 'across)
               anything-parser))

(add-for-as-subclause-parser for-as-across-parser)
