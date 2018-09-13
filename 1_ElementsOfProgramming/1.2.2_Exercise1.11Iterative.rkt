#lang sicp

(define (f n)
  (f-iter n 0 0 0))
(define (f-iter n f-n-1 f-n-2 f-n-3)
  (cond 
          