#lang sicp
(#%require (only racket/base
                 print-as-expression
                 print-pair-curly-braces
                 print-mpair-curly-braces))
(print-as-expression      #f)
(print-pair-curly-braces  #t)
(print-mpair-curly-braces #f)

(cons (list 1 2) (list 3 4))

(define x (cons (list 1 2) (list 3 4)))
(length x)

(define (count-leaves x)
  (cond ((null? x) 0)
           ((not (pair? x)) 1)
           (else (+ (count-leaves (car x))
                       (count-leaves (cdr x))))))
(count-leaves x)

; Like scale-list procedure only now factor each leave:
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
           ((not (pair? tree)) (* tree factor))
           (else (cons (scale-tree (car tree) factor)
                           (scale-tree (cdr tree) factor)))))
(scale-tree x 2)

; Using map:
(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
             (if (pair? sub-tree)
                 (scale-tree sub-tree factor)
                 (* sub-tree factor)))
           tree))
(scale-tree-map x 2)