;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 21 07:08:21 2002
;;;; Contains: Tests that keywords can be loop keywords

;;; Tests of loop keywords

(deftest loop.15.30
  (loop :for i :from 1 :to 10 :collect i)
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.15.31
  (loop :for i :upfrom 1 :below 10 :by 2 :collect i)
  (1 3 5 7 9))

(deftest loop.15.32
  (loop :with x = 1 :and y = 2 :return (values x y))
  1 2)

;(deftest loop.15.33
;  (loop :named foo :doing (return-from foo 1))
;  1)

(deftest loop.15.34
  (let ((x 0))
    (loop
     :initially (set! x 2)
     :until #t
     :finally (return x)))
  2)

(deftest loop.15.35
  (loop :for x :in '(a b c) :collecting x)
  (a b c))

(deftest loop.15.36
  (loop :for x :in '(a b c) :append (list x))
  (a b c))

(deftest loop.15.37
  (loop :for x :in '(a b c) :appending (list x))
  (a b c))

(deftest loop.15.38
  (loop :for x :in '(a b c) :nconc (list x))
  (a b c))

(deftest loop.15.39
  (loop :for x :in '(a b c) :nconcing (list x))
  (a b c))

(deftest loop.15.40
  (loop :for x :in '(1 2 3) :count x)
  3)

(deftest loop.15.41
  (loop :for x :in '(1 2 3) :counting x)
  3)

(deftest loop.15.42
  (loop :for x :in '(1 2 3) :sum x)
  6)

(deftest loop.15.43
  (loop :for x :in '(1 2 3) :summing x)
  6)

(deftest loop.15.44
  (loop :for x :in '(10 20 30) :maximize x)
  30)

(deftest loop.15.45
  (loop :for x :in '(10 20 30) :maximizing x)
  30)

(deftest loop.15.46
  (loop :for x :in '(10 20 30) :minimize x)
  10)

(deftest loop.15.47
  (loop :for x :in '(10 20 30) :minimizing x)
  10)

(deftest loop.15.48
  (loop :for x :in '(1 2 3 4) :sum x :into foo :of-type fixnum
        :finally (return foo))
  10)

(deftest loop.15.49
  (loop :for x :upfrom 1 :to 10
        :if (even? x) :sum x :into foo
        :else :sum x :into bar
        :end
        :finally (return (values foo bar)))
  30 25)

(deftest loop.15.50
  (loop :for x :downfrom 10 :above 0
        :when (even? x) :sum x :into foo
        :else :sum x :into bar
        :end
        :finally (return (values foo bar)))
  30 25)

(deftest loop.15.51
  (loop :for x :in '(a b #f c d #f)
        :unless x :count #t)
  2)

(deftest loop.15.52
  (loop :for x :in '(a b #f c d #f)
        :unless x :collect x :into bar :and :count #t :into foo
        :end
        finally (return (values bar foo)))
  (#f #f)
  2)

(deftest loop.15.53
  (loop :for x :in '(#f #f a b #f c #f)
        :collect x
        :until x)
  (#f #f a))

(deftest loop.15.54
  (loop :for x :in '(a b #f c #f)
        :while x :collect x)
  (a b))

(deftest loop.15.55
  (loop :for x :in '(#f #f a b #f c #f)
        :thereis x)
  a)

(deftest loop.15.56
  (loop :for x :in '(#f #f a b #f c #f)
        :never x)
  #f)

(deftest loop.15.57
  (loop :for x :in '(a b c d e)
        :always x)
  #t)

(deftest loop.15.58
  (loop :as x :in '(a b c) :count #t)
  3)

(deftest loop.15.59
  (loop :for i :from 10 :downto 5 :collect i)
  (10 9 8 7 6 5))

(deftest loop.15.60
  (loop :for i :from 0 :upto 5 :collect i)
  (0 1 2 3 4 5))

(deftest loop.15.61
  (loop :for x :on '(a b c) :collecting (car x))
  (a b c))

(deftest loop.15.62
  (loop :for x = '(a b c) :then (cdr x)
        :while (not (null? x))
        :collect (car x))
  (a b c))

(deftest loop.15.63
  (loop :for x :across #(a b c) :collect x)
  (a b c))

(deftest loop.15.64
  (loop :for () :across (make-hash-table)
        :count #t)
  0)
