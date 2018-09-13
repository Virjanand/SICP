#lang sicp

(define (square x) (* x x))

(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) (* guess 0.001)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x) guess
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))