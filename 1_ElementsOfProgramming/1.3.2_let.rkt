#lang sicp

(define (f x y)
  (let ((a (+ 1 (* x y)))
         (b (- 1 y)))
    (+ (* x (square a))
        (* y b)
        (* a b))))

(define (square n)
  (* n n))

(define x 5)
(+ (let ((x 3))
       (+ x (* x 10)))
    x)

(define x2 2)
(let ((x2 3)
       (y (+ x2 2)))
  (* x2 y))