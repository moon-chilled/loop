;;; This function is called in a SUM clause in order to sum the
;;; accumulated value with the new one.
(define (sum x . y)
  (for-each (lambda (y)
              (unless (number? y)
                (error 'sum-argument-must-be-number
                       :datum y
                       :expected-type 'number))
              (set! x (+ x y)) y)
            y)
  x)

;;; This function is called in MAX and MIN clauses to ensure that new values
;;; are real.
(define (ensure-real x what)
  (unless (real? x)
    (error what
           :datum x
           :expected-type 'real))
  x)


;;; This function is called in a MAX clause in order to compute the
;;; max of the accumulated value and the new one.
(define (maximize x . y)
  (for-each (lambda (y)
              (set! x (max x (ensure-real y 'max-argument-must-be-real))))
            y)
  x)

;;; This function is called in a MIN clause in order to compute the
;;; min of the accumulated value and the new one.
(define (minimize x . y)
  (for-each (lambda (y)
              (set! x (min x (ensure-real y 'min-argument-must-be-real))))
            y)
  x)

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

(define (last x)
  (if (null? x)
    ()
    (if (not (pair? (cdr x)))
      x
      (last (cdr x)))))

(define (copying-append xs)
  (if (null? xs)
    ()
    (let ((t (last xs)))
      (set-car! t (copy (car t)))
      (apply append xs))))

(define (copy-and-last x)
  (let* ((ret (list (car x)))
         (head ret)
         (tail (cdr x)))
    (let loop ()
      (if (not (pair? tail))
        (cons ret head)
        (begin
          (set! head (set! (cdr head) (list (car tail))))
          (set! tail (cdr tail)))))))

(define (nconc xs)
  (if (null? xs)
    ()
    (if (null? (cdr xs))
      (car xs)
      (let* ((ret (list ()))
             (tail ret))
        (let loop ((cur xs))
          (if (null? (cdr cur))
            (set! (cdr tail) (car cur))
            (let ((c (car cur)))
              (if (null? c)
                (loop (cdr cur))
                (let ((cl (copy-and-last c)))
                  (set! (cdr tail) (car cl))
                  (set! tail (cdr cl)))))))
        (cdr ret)))))
