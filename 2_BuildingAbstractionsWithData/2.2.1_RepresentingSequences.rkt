#lang sicp
(#%require (only racket/base
                 print-as-expression
                 print-pair-curly-braces
                 print-mpair-curly-braces))
(print-as-expression      #f)
(print-pair-curly-braces  #t)
(print-mpair-curly-braces #f)

(cons 1
        (cons 2
                (cons 3
                         (cons 4 nil))))

(list 1 2 3 4)
(define one-through-four (list 1 2 3 4))
one-through-four
(cdr one-through-four)

;list-ref takes list and number n and returns the nth item (start with 0)
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3)

;length of list using primitive predicate null?
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

; length iterative
(define (length-iterative items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(length-iterative odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append squares odds)

;Mapping over lists:
;scale all elements in list with factor
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
               (scale-list (cdr items) factor))))
(scale-list odds 2)

;Abstract  higher-order procedure called map
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
               (map proc (cdr items)))))

(define (scale-list-map items factor)
  (map (lambda (x) (* x factor))
          items))
(scale-list-map odds 2)