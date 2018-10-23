#lang sicp
(#%require (only racket/base
                 print-as-expression
                 print-pair-curly-braces
                 print-mpair-curly-braces))
(print-as-expression      #f)
(print-pair-curly-braces  #t)
(print-mpair-curly-braces #f)

; Take a tree and compute the sum of the squares of the leaves that are odd:
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
           ((not (pair? tree))
             (if (odd? tree) (square tree) 0))
           (else (+ (sum-odd-squares (car tree))
                       (sum-odd-squares (cdr tree))))))

(define (square x)
  (* x x))

; List all even Fibonacci numbers where k is less than or equal to integer n:
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; Filter a sequence to select only those elements that satisfy a given predicate:
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
           ((predicate (car sequence))
            (cons (car sequence)
                     (filter predicate (cdr sequence))))
            (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

; Accumulations:
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
           (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

; Enumerate sequence of integers in a given range:
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(enumerate-interval 2 7)

; Enumerate leaves of a tree:
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
           ((not (pair? tree)) (list tree))
           (else (append (enumerate-tree (car tree))
                               (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

; Reformulate sum-odd-squares-tree and even-fibs as in signal-flow diagrams

(define (sum-odd-squares-seq tree)
  (accumulate +
                     0
                    (map square
                            (filter odd?
                                     (enumerate-tree tree)))))
(define x (list 1 2 (list 3 4)))
(sum-odd-squares x)
(sum-odd-squares-seq x)

(define (even-fibs-seq n)
  (accumulate cons
                    nil
                    (filter even?
                             (map fib
                                     (enumerate-interval 0 n)))))
(even-fibs 10)
(even-fibs-seq 10)

; We can reuse pieces from the sum-odd-squares and even-fibs to construct
; a list of squares of the first n+1 Fibonacci numbers:

(define (list-fib-squares n)
  (accumulate cons
                    nil
                    (map square
                            (map fib
                                     (enumerate-interval 0 n)))))
(list-fib-squares 10)

; Or compute the product of odd integers in a sequence:
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
                    1
                    (map square
                            (filter odd? sequence))))
(define l (list 1 2 3 4 5))
(product-of-squares-of-odd-elements l)