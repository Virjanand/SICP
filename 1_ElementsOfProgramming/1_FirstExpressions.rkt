#lang sicp


(define size 2)
size
(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

(define (square x) (* x x))

(square 21)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0)(- x))))

(abs -2)

(define (>= x y)
  (or (> x y) (= x y)))

(>= 7 7)

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 7 6 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

(/ (+ 5
      (/ 1 2)
      (- 2 (- 3
              (+ 6
                 (/ 1 3)))))
   (* 3
      (- 6 2)
      (- 2 7)))

(define c 5)
(define (two-largest-sum-of-squares a b c)
  (cond ((and (< a b)
             (< a c))
        (sum-of-squares b c))
        ((and (< b a)
             (< b c))
        (sum-of-squares a c))
        (else (sum-of-squares a b))))


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
