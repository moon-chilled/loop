;;; This function is called in a SUM clause in order to sum the
;;; accumulated value with the new one.
(define (sum x y)
  (unless (number? y)
    (error 'sum-argument-must-be-number
           :datum y
           :expected-type 'number))
  (+ x y))

;;; This function is called in a MAX clause in order to compute the
;;; max of the accumulated value and the new one.
(define (maximize x y)
  (unless (real? y)
    (error 'max-argument-must-be-real
           :datum y
           :expected-type 'real))
  (max x y))

;;; This function is called in a MIN clause in order to compute the
;;; min of the accumulated value and the new one.
(define (minimize x y)
  (unless (real? y)
    (error 'min-argument-must-be-real
           :datum y
           :expected-type 'real))
  (min x y))

;;; This function is called during restructuring to compute the CAR of
;;; some value.  If that value turns out not to be a LIST, then an
;;; error is signaled.
(define (list-car x)
  (if (pair? x)
      (car x)
      (error 'value-must-be-list
             :datum x
             :expected-type 'list)))

;;; This function is called during restructuring to compute the CDR of
;;; some value.  If that value turns out not to be a LIST, then an
;;; error is signaled.
(define (list-cdr x)
  (if (pair? x)
      (cdr x)
      (error 'value-must-be-list
             :datum x
             :expected-type 'list)))

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))
