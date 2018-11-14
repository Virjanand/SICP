#lang sicp
(#%require (only racket/base
                 print-as-expression
                 print-pair-curly-braces
                 print-mpair-curly-braces))
(print-as-expression      #f)
(print-pair-curly-braces  #t)
(print-mpair-curly-braces #f)

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)

(car '(a b c))

(cdr '(a b c))

; Check if symbol in list
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))


; Differentiation rules
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error *unknown expression type -- DERIV* exp))))

; Representing algebraic expressions
(define (variable?) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddrs s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))