#lang sicp

(define (square x) (* x x))

(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) (* guess 0.001)))

(define (improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (sqcb-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqcb-iter (improve guess x) guess
                 x)))

(define (sqcb x)
  (sqcb-iter 1.0 2.0 x))