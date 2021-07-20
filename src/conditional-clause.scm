(defclass conditional-clause (selectable-clause)
  (condition then-clauses else-clauses)

  ;;; A conditional clause does not introduce any bindings for any
  ;;; variables, so this method should return the empty list.
  (bound-variables (clause)
    '())

  (accumulation-variables (clause)
    (append (apply append
                    (map accumulation-variables (clause 'then-clauses)))
            (apply append
                    (map accumulation-variables (clause 'else-clauses)))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; Compute body-form.

  (body-form (clause end-tag)
    (let-temporarily ((*it-var* (gensym)))
      `(let ((,*it-var* ,(clause 'condition)))
         (if ,*it-var*
             (begin
               ,@(map (lambda (clause)
                           (body-form clause end-tag))
                         (clause 'then-clauses)))
             (begin
               ,@(map (lambda (clause)
                           (body-form clause end-tag))
                         (clause 'else-clauses))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser then-or-else-parser
  (consecutive cons
               selectable-clause-parser
               (repeat* list
                        and-selectable-clause-parser)))
(define-parser if-else-end-clause-parser
  (consecutive (lambda (if form then-clauses else else-clauses end)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser
               (keyword-parser 'else)
               then-or-else-parser
               (keyword-parser 'end)))

(define-parser if-end-clause-parser
  (consecutive (lambda (if form then-clauses end)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses '()))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser
               (keyword-parser 'end)))

(define-parser if-else-clause-parser
  (consecutive (lambda (if form then-clauses else else-clauses)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser
               (keyword-parser 'else)
               then-or-else-parser))

(define-parser if-clause-parser
  (consecutive (lambda (if form then-clauses)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses '()))
               (alternative (keyword-parser 'if)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser))

(define-parser if-when-parser
  (alternative if-else-end-clause-parser
               if-end-clause-parser
               if-else-clause-parser
               if-clause-parser))

(define-parser unless-else-end-clause-parser
  (consecutive (lambda (unless form else-clauses else then-clauses end)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               anything-parser
               then-or-else-parser
               (keyword-parser 'else)
               then-or-else-parser
               (keyword-parser 'end)))

(define-parser unless-end-clause-parser
  (consecutive (lambda (unless form else-clauses end)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses '()
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               anything-parser
               then-or-else-parser
               (keyword-parser 'end)))

(define-parser unless-else-clause-parser
  (consecutive (lambda (unless form else-clauses else then-clauses)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword-parser 'unless)
               anything-parser
               then-or-else-parser
               (keyword-parser 'else)
               then-or-else-parser))

(define-parser unless-clause-parser
  (consecutive (lambda (unless form else-clauses)
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses '()
                   :else-clauses else-clauses))
               (alternative (keyword-parser 'unless)
                            (keyword-parser 'when))
               anything-parser
               then-or-else-parser))

(define-parser unless-parser
  (alternative unless-else-end-clause-parser
               unless-end-clause-parser
               unless-else-clause-parser
               unless-clause-parser))

(define-parser conditional-clause-parser
  (alternative if-when-parser
               unless-parser))

(add-clause-parser conditional-clause-parser)

