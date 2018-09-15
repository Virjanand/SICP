#lang sicp

(define (f n)
  (f-iter n 0 0 0 0))
(define (f-iter n counter f-n-1 f-n-2 f-n-3)
  (cond ((and (= counter 0)
              (<= counter n)) (f-iter n
                               (+ counter 1)
                               0
                               0
                               counter))
        ((and (= counter 1)
              (<= counter n)) (f-iter n
                               (+ counter 1)
                               counter
                               counter
                               f-n-3))
        ((and (= counter 2)
              (<= counter n)) (f-iter n
                               (+ counter 1)
                               counter
                               f-n-2
                               f-n-3))
        ((and (>= counter 3)
              (<= counter n)) (f-iter n
                               (+ counter 1)
                               (+ f-n-1
                                  (* 2 f-n-2)
                                  (* 3 f-n-3))
                               f-n-1
                               f-n-2))
        (else f-n-1)))

(define (f2 n)
  (f-iter2 n 0 1 2))
(define (f-iter2 n f-n-1 f-n-2 f-n-3)
  (if (= n 0)
      f-n-1
      (f-iter2 (- n 1)
               f-n-2
               f-n-3
              (+ f-n-3
                 (* 2 f-n-2)
                 (* 3 f-n-1)))))