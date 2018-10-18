#lang sicp

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
          a
          (lambda (x) (+ x 4))
          b))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
          (sum term (next a) next b))))

(define (square n)
  (* n n))

((lambda (x y z) (+ x y (square z))) 1 2 3)
