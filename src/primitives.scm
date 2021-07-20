(define-macro (push v s)
  (unless (symbol? s) (error))
  `(begin (set! ,s (cons ,v ,s)) ,s))

(define (list* . p)
  (if (null? (cdr p))
    (car p)
    (cons (car p) (apply list* (cdr p)))))

; a la j: x f&g y ←→ (f x) g (f y)
; hence, ((& f g) x y z...) ←→ (f (g x) (g y) (g z)...)
(define (compose f g) (lambda args (apply f (map g args))))
; and here: x (f g) y ←→ x f g y
; ((hook f g) x y) ←→ (f x (g y))
(define (hook f g) (lambda (x y) (f x (g y))))
; in particular, cl (higher-order-fn :pred f :key g)
; can be expressed here using simply (higher-order fn :pred (hook f g))
(define (bind f . rest) (lambda args (apply f (append rest args))))
(define (rbind f . rest) (lambda args (apply f (append args rest))))

(define (filter f xs)
  (cond
    ((null? xs) '())
    ((f (car xs)) (cons (car xs) (filter f (cdr xs))))
    (#t (filter f (cdr xs)))))
(define (any f l) (not (null? (filter f l))))

(define (constantly val)
  (lambda - val))

(define (identity x) x)

(define (remove-duplicates l pred)
  (let ((ret '()))
    (let loop ((l l))
      (unless (null? l)
        (unless (any (lambda (x) (pred x (car l))) ret)
          (push (car l) ret))
        (loop (cdr l))))
    (reverse ret)))

(define (remove-duplicates-from-end l pred)
  (let ((ret '()))
    (let loop ((l l))
      (unless (null? l)
        (loop (cdr l))
        (unless (any (lambda (x) (pred x (car l))) ret)
          (push (car l) ret))))
    ret))

(define (every f l)
  (if (null? l)
    #t
    (and (f (car l))
         (every f (cdr l)))))

(define (position-if pred l)
  (let loop ((l l)
             (i 0))
    (cond
      ((null? l) #f)
      ((pred (car l)) i)
      (#t (loop (cdr l) (+ 1 i))))))

(define (position-if-from-end pred l)
  (let loop ((l l)
             (i 0))
    (cond ((null? l) #f)
          ((pred (car l)) (or (loop (cdr l) (+ 1 i)) i))
          (#t (loop (cdr l) (+ 1 i))))))

(define (for-each-on f xs)
  (if (null? xs)
    '()
    (begin
      (f xs)
      (for-each-on f (cdr xs)))))

(define (count x xs pred)
  (let loop ((xs xs)
             (acc 0))
    (if (null? xs) acc
      (loop (cdr xs) (+ acc (if (pred x (car xs)) 1 0))))))

(define (intersection x y pred)
  (filter (rbind member y pred) x))

; to make up for multiple-value-bind
(define-macro (pidgin-destructuring-bind spec var . body)
  (let ((v (gensym)))
    (letrec ((expand (lambda (spec)
                       (if (null? spec)
                         `((unless (null? ,v) (error "too many values specified for destructuring")))
                         (list* `(set! ,(car spec) (car ,v))
                                `(set! ,v (cdr ,v))
                                (expand (cdr spec)))))))
      `(let ((,v ,var)
             ,@(map (lambda (s) `(,s #f)) spec))
         ,@(expand spec)
         ,@body))))

; simple stub generics implementation (single dispatch only)
; (defgeneric m (x y z) body*)  will define a function 'm' that expects 'x' to
; be a let containing a bound lambda 'm'; y and z will be passed to that
; lambda.  If 'm' is not bound, body will be evaluated instead.  If body is
; nil, an error will be signaled

(define-macro (defgeneric name pspec . body)
  `(define (,name ,@pspec)
     (if (eq? ,name (,(car pspec) ',name))
       ,(if (null? body) `(error "No method ~a bound in class ~a" ',name (,(car pspec) 'class-name)) `(begin ,@body))
       ((,(car pspec) ',name) ,@pspec))))

(define *classes* (make-hash-table 8 eq?))

(define (type-specifier? x) (or (symbol? x) (eq? x #f)))

(define-macro (defclass name super slots . methods)
  (when (*classes* name) (error "Class already defined: ~a" name))
  (let* ((super (map (lambda (s) (let ((r (*classes* s))) (unless r (error "No superclass ~a" s)) r)) super))
         (auxiliary-slots   (apply append (map (lambda (c) (c 'all-slots)) super)))
         (auxiliary-methods (apply append (map (lambda (c) (c 'all-methods)) super)))
         (methods (map (lambda (m) `(,(car m) (lambda* ,(cadr m) ,@(cddr m)))) methods))
         (slots (map (lambda (s) (if (pair? s) s `(,s (error ,(format #f "No initializer supplied for slot ~a" s))))) slots))
         (accessor-methods '())
         (all-slots (remove-duplicates-from-end `(,@auxiliary-slots ,@slots) (compose eq? car)))
         (all-methods (remove-duplicates-from-end `(,@auxiliary-methods ,@accessor-methods ,@methods) (compose eq? car)))
         (classes (cons name (remove-duplicates (apply append (map (lambda (x) (x 'classes)) super)) eq?))))
    `(set! (*classes* ',name)
           (inlet 'all-slots ',all-slots
                  'all-methods ',all-methods
                  'class-name ',name
                  'classes ',classes
                  'make (lambda* ,all-slots
                          (let ((class-name ',name)
                                ,@all-methods)
                            (curlet)))))))

(define (make-instance what . p)
  (apply ((*classes* what) 'make) p))

(define (type? var type) (member type ((*classes* (var 'class-name)) 'classes)))
