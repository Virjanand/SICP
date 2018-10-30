#lang sicp
(#%require (only racket/base
                 print-as-expression
                 print-pair-curly-braces
                 print-mpair-curly-braces))
(print-as-expression      #f)
(print-pair-curly-braces  #t)
(print-mpair-curly-braces #f)

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define frame (make-frame (list 1 1) (list 2 0) (list 0 2)))
(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)

; vector
(define (make-vect x y)
  (list x y))
(define vect (make-vect 0 1))

(define (xcor-vect vect)
  (car vect))
(xcor-vect vect)

(define (ycor-vect vect)
  (car (cdr vect)))
(ycor-vect vect)

; vector calculation
(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1)
                (xcor-vect vect2))
             (+ (ycor-vect vect1)
                (ycor-vect vect2))))
(define vect2 (make-vect 1 0))
(add-vect vect vect2)

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1)
                (xcor-vect vect2))
             (- (ycor-vect vect1)
                (ycor-vect vect2))))
(sub-vect vect vect2)

(define (scale-vect scale vect)
  (make-vect (* scale
                (xcor-vect vect))
             (* scale
                (ycor-vect vect))))
(scale-vect 5 vect)

; frame coordinate map
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
((frame-coord-map frame) (make-vect 0 0))
(origin-frame frame)