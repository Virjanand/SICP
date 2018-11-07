#lang sicp
(#%require sicp-pict)
(#%require (only racket/base
                 print-as-expression
                 print-pair-curly-braces
                 print-mpair-curly-braces))
(print-as-expression      #f)
(print-pair-curly-braces  #t)
(print-mpair-curly-braces #f)
(#%require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;need a wrapper function so that the graphics library works with my code...
(define (vector-to-posn v)
  (make-posn (car v) (car(cdr v))))

; frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define frame (make-frame (list 0 0) (list 1 0) (list 0 1)))
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

; segment
(define (make-segment start-vect end-vect)
  (list start-vect end-vect))
(define segment (make-segment vect vect2))

(define (start-segment segment)
  (car segment))
(start-segment segment)

(define (end-segment segment)
  (car (cdr segment)))
(end-segment segment)

(define (segments->painter segment-list)   
  (lambda (frame)     
   (for-each     
     (lambda (segment)        
      (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list)))

(define outline-segments
 (list
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.0 0.5))
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.5 0.0))
  (make-segment
   (make-vect 0.5 0.0)
   (make-vect 0.5 0.5))
  (make-segment
   (make-vect 0.0 0.5)
   (make-vect 0.5 0.5))))

(define outline (segments->painter outline-segments))
(outline frame)