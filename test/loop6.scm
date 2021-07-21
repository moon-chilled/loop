;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Nov 10 21:13:04 2002
;;;; Contains: Tests for LOOP-AS-HASH forms

(define (symbol< s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define *loop.6.alist*
  '((a . 1) (b . 2) (c . 3)))

(define *loop.6.alist.2*
  '(("a" . 1) ("b" . 2) ("c" . 3)))

(define *loop.6.alist.3*
  '(((a1 . a2) . 1) ((b1 . b2) . 2) ((c1 . c2) . 3)))

(define *loop.6.hash.1*
  (let ((table (make-hash-table 8 eq?)))
    (loop for (key . val) in *loop.6.alist*
          do (set! (table key) val))
    table))

(define *loop.6.hash.2*
  (let ((table (make-hash-table 8 eqv?)))
    (loop for (key . val) in *loop.6.alist*
          do (set! (table key) val))
    table))

(define *loop.6.hash.3*
  (let ((table (make-hash-table 8 equal?)))
    (loop for (key . val) in *loop.6.alist.3*
          do (set! (table key) val))
    table))

;;; (define *loop.6.hash.4*
;;;  (let ((table (make-hash-table 8 equivalent?)))
;;;    (loop for (key . val) in *loop.6.alist.2*
;;;       do (set! (table key) val))
;;;    table))

(define *loop.6.hash.5*
  (let ((table (make-hash-table 8 eqv?)))
    (loop for (val . key) in *loop.6.alist.3*
          do (set! (table key) val))
    table))

(define *loop.6.hash.6*
  (let ((table (make-hash-table 8 eq?)))
    (loop for (key . val) in *loop.6.alist*
          do (set! (table key) (+ val 0.0)))
    table))

(define *loop.6.hash.7*
  (let ((table (make-hash-table 8 equal?)))
    (loop for (val . key) in *loop.6.alist.3*
          do (set! (table (+ key 0.0)) val))
    table))

(define *loop.6.alist.8*
  '(((1 . 2) . 1) ((3 . 4) . b) ((5 . 6) . c)))

(define *loop.6.hash.8*
  (let ((table (make-hash-table 8 equal?)))
    (loop for (key . val) in *loop.6.alist.8*
          do (set! (table key) val))
    table))

(define *loop.6.hash.9*
  (let ((table (make-hash-table 8 equal?)))
    (loop for (val . key) in *loop.6.alist.8*
          do (set! (table key) val))
    table))

;;; being {each | the} {hash-value | hash-values | hash-key | hash-keys} {in | of }

(deftest loop.6.1
  (loop for (() . x) across *loop.6.hash.1* sum x)
  6)

(deftest loop.6.2
  (loop for (() . x) across *loop.6.hash.1* sum x)
  6)

(deftest loop.6.3
  (loop for (() . x) across *loop.6.hash.1* sum x)
  6)

(deftest loop.6.4
  (loop for (() . x) across *loop.6.hash.1* sum x)
  6)

(deftest loop.6.5
  (loop for (() . x) across *loop.6.hash.1* sum x)
  6)

(deftest loop.6.6
  (sort! (loop for (x) across *loop.6.hash.1* collect x)
        symbol<)
  (a b c))

(deftest loop.6.7
  (sort! (loop for (x) across *loop.6.hash.1* collect x)
        symbol<)
  (a b c))

(deftest loop.6.8
  (sort! (loop for (x) across *loop.6.hash.1* collect x)
        symbol<)
  (a b c))

(deftest loop.6.9
  (sort! (loop for (x) across *loop.6.hash.1* collect x)
        symbol<)
  (a b c))

(deftest loop.6.10
  (sort! (loop for (x) across *loop.6.hash.1* collect x)
        symbol<)
  (a b c))

(deftest loop.6.11
  (sort! (loop for ((u . v)) across *loop.6.hash.3* collect u)
        symbol<)
  (a1 b1 c1))

(deftest loop.6.12
  (sort! (loop for ((u . v)) across *loop.6.hash.3* collect v)
        symbol<)
  (a2 b2 c2))

(deftest loop.6.13
  (sort! (loop for (() . (u . v)) across *loop.6.hash.5* collect u)
        symbol<)
  (a1 b1 c1))

(deftest loop.6.14
  (sort! (loop for (() . (u . v)) across *loop.6.hash.5* collect v)
        symbol<)
  (a2 b2 c2))

(deftest loop.6.15
  (sort! (loop for (k . v) across *loop.6.hash.1*
              collect (list k v))
         (lambda (x y) (< (cadr x) (cadr y))))
  ((a 1) (b 2) (c 3)))

(deftest loop.6.17
  (sort! (loop for (() . (u . ())) across *loop.6.hash.5* collect u)
        symbol<)
  (a1 b1 c1))

(deftest loop.6.18
  (sort! (loop for (() . (() . v)) across *loop.6.hash.5* collect v)
        symbol<)
  (a2 b2 c2))

(deftest loop.6.19
  (loop for () across *loop.6.hash.5* count #t)
  3)

(deftest loop.6.20
  (loop for (()) across *loop.6.hash.5* count #t)
  3)

(deftest loop.6.21
  (loop for (() . v) across *loop.6.hash.5* count #t)
  3)

(deftest loop.6.22
  (loop for (k) across *loop.6.hash.5* count #t)
  3)

(deftest loop.6.23
  (loop for (() . v) of-type (t . fixnum) across *loop.6.hash.1* sum v)
  6)

(deftest loop.6.25
  (loop for (k) of-type (fixnum) across *loop.6.hash.5* sum k)
  6)

(deftest loop.6.27
  (loop for (k) of-type (t) across *loop.6.hash.5* sum k)
  6)

(deftest loop.6.29
  (loop for (() . v) of-type (t . t) across *loop.6.hash.1* sum v)
  6)

(deftest loop.6.31
  (loop for (() . v) of-type (t . float) across *loop.6.hash.6* sum v)
  6.0)

(deftest loop.6.33
  (loop for (k) of-type (float) across *loop.6.hash.7* sum k)
  6.0)

(deftest loop.6.35
  (loop for ((k1 . k2)) of-type ((integer . integer))
        across *loop.6.hash.8* sum (+ k1 k2))
  21)

(deftest loop.6.36
  (loop for (() . (v1 . v2)) of-type (t . (integer . integer))
        across *loop.6.hash.9* sum (+ v1 v2))
  21)

(deftest loop.6.37
  (loop for ((k1 . k2) . v) across *loop.6.hash.8* sum (+ k1 k2))
  21)

(deftest loop.6.38
  (loop for (k . (v1 . v2)) across *loop.6.hash.9* sum (+ v1 v2))
  21)

(deftest loop.6.39
  (loop as (() . x) across *loop.6.hash.1* sum x)
  6)

(deftest loop.6.40
  (sort! (loop as (x) across *loop.6.hash.1* collect x)
        symbol<)
  (a b c))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

;(deftest loop.6.41
;  (macrolet
;   ((%m (z) z))
;   (loop for x being the hash-value of
;         (expand-in-current-env (%m *loop.6.hash.1*)) sum x))
;  6)
;
;(deftest loop.6.42
;  (macrolet
;   ((%m (z) z))
;   (sort! (loop for x being the hash-key of
;               (expand-in-current-env (%m *loop.6.hash.1*)) collect x)
;         symbol<))
;  (a b c))
