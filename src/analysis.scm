;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntactic and semantic analysis

;;; Check that if there is a name-clause, the last one is in position
;;; zero.
(define (check-name-clause-position clauses)
  (let ((name-clause-position
          (position-if-from-end (rbind type? 'name-clause) clauses)))
    (when (and name-clause-position (positive? name-clause-position))
      (error 'name-clause-not-first))))

;;; Check that there is not a variable-clause following a main clause.
;;; Recall that we diverge from the BNF grammar in the HyperSpec so
;;; that INITIALLY and FINALLY are neither main clauses nor variable
;;; clauses.
(define (check-order-variable-clause-main-clause clauses)
  (let ((last-variable-clause-position
          (position-if-from-end (rbind type? 'variable-clause) clauses))
        (first-main-clause-position
          (position-if (rbind type? 'main-clause) clauses)))
    (when (and last-variable-clause-position
               first-main-clause-position
               (> last-variable-clause-position first-main-clause-position))
      (error 'invalid-clause-order))))

(define (verify-clause-order clauses)
  (check-name-clause-position clauses)
  (check-order-variable-clause-main-clause clauses))

(define (check-variable-uniqueness clauses)
  (let* ((variables (apply append (map bound-variables clauses)))
         (unique-variables (remove-duplicates variables eq?)))
    (unless (= (length variables)
               (length unique-variables))
      (map (lambda (var) (when (> (count var variables eq?) 1)
                           (error 'multiple-variable-occurrences
                                  :bound-variable var)))
           unique-variables))))

;;; Check that for a given accumulation variable, there is only one
;;; category.  Recall that the accumlation categores are represented
;;; by the symbols LIST, COUNT/SUM, and MAX/MIN.
(define (check-accumulation-categories clauses)
  (let* ((descriptors (apply append (map accumulation-variables clauses)))
         (equal-fun (lambda (d1 d2)
                      (and (eq? (car d1) (car d2))
                           (eq? (cadr d1) (cadr d2)))))
         (unique (remove-duplicates descriptors equal-fun)))
    (for-each-on (lambda (remaining)
                   (let ((entry (member (caar remaining)
                                (cdr remaining)
                                (hook eq? car))))
                     (when entry
                       (error "the accumulation variable ~s is used both for ~s accumulation and ~s accumulation."
                              (caar remaining)
                              (cdar remaining)
                              (cdar entry)))))
                 unique)))

;;; Check that there is no overlap between the bound variables and the
;;; accumulation variables.
(define (check-no-variable-overlap clauses)
  (let ((bound-variables
          (apply append (map bound-variables clauses)))
        (accumulation-variables
          (map car
               (apply append
                      (map accumulation-variables clauses)))))
    (let ((intersection
            (intersection bound-variables accumulation-variables
                          eq?)))
      (unless (null? intersection)
        (error "The variable ~s is used both as an iteration variable and as an accumulation variable."
               (car intersection))))))

;;; FIXME: Add more analyses.
(define (analyze-clauses clauses)
  (verify-clause-order clauses)
  (check-variable-uniqueness clauses)
  (check-accumulation-categories clauses)
  (check-no-variable-overlap clauses))
