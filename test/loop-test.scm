(load "../loop.scm")

(define-macro (deftest name form . results)
  (let ((x (gensym)))
    `(let ((,x (list-values ,form)))
       (if (equivalent? ,x ',results)
         (format #t "pass ~a~%" ',name)
         (error "Test ~a failed; returned ~a but was supposed to return ~a" ',name ,x ',results)))))

(define-macro* (incf x (acc 1))
  `(set! ,x (+ ,x ,acc)))
(define-macro* (decf x (acc 1))
  `(set! ,x (- ,x ,acc)))

;(defmacro signals-error (form error-type)
;  `(handler-case (eval ,form)
;     (,error-type () t)
;     (condition () nil)
;     (:no-error (&rest values) (declare (ignore values)) nil)))


; TODO
;(deftest loop-finish-in-simple-loop
;    (loop do (loop (loop-finish)))
;  '())

(deftest loop-until-t
    (loop until #t)
  ())

(deftest loop-while-nil
    (loop while #f)
  ())

(deftest loop-until-expr
    (let ((n 0))
      (loop until (= (incf n) 10))
      n)
  10)

(deftest loop-while-expr
    (let ((n 0))
      (loop while (< (incf n) 10))
      n)
  10)

(deftest loop-do
    (let ((n 0))
      (call-with-exit (lambda (abc)
        (loop do (begin (incf n)
                        (when (= n 10)
                          (abc))))))
      n)
  10)

(deftest loop-until-do
  (let ((n 0))
    (loop until (= n 10)
          do (incf n))
    n)
  10)

(deftest loop-repeat-do
  (let ((n 0))
    (loop repeat 5
          do (incf n 2))
    n)
  10)

(deftest loop-with-repeat-do
  (let ((n 0))
    (loop with step = 2
          repeat 5
          do (incf n step))
    n)
  10)

(deftest loop-initially-repeat-do
  (let ((n 0))
    (loop initially (incf n 10)
          repeat 2
          do (set! n (* n 2)))
    n)
  40)

(deftest loop-repeat-do-finally
  (let ((n 0))
    (loop repeat 2
          do (incf n 10)
          finally (set! n (* n 2)))
    n)
  40)

(load "loop0.scm")
(load "loop1.scm")
(load "loop2.scm")
(load "loop3.scm")
(load "loop4.scm")
(load "loop5.scm")
(load "loop6.scm")
(load "loop8.scm")
(load "loop9.scm")

; commented in sicl:
;; (defun loop-with-repeat-do-collect-finally ()
;;   (assert (equal (let ((result nil))
;;                 (loop with n = 0
;;                       repeat 4
;;                       do (incf n)
;;                       collect n into foo
;;                       finally (setf result foo))
;;                 result)
;;               '(1 2 3 4))))

;; (defun loop-repeat-sum-finally ()
;;   (assert (equal (let ((result nil))
;;                 (loop repeat 5
;;                       sum 2 into foo
;;                       finally (setf result foo))
;;                 result)
;;               10)))
