#lang sicp
(#%require (only racket/base
                 print-as-expression
                 print-pair-curly-braces
                 print-mpair-curly-braces))
(print-as-expression      #f)
(print-pair-curly-braces  #t)
(print-mpair-curly-braces #f)

(define (make-frame origin edge1 edge2)
  (list edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

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
  (cadr vect))
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



; transform painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

; flip painter vertically
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)  ; new origin
                     (make-vect 1.0 1.0)  ; new end of edge1
                     (make-vect 0.0 0.0))); new end of edge2

; shrink image to upper-right quarter of frame
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

; rotate image counterclockwise by 90 degrees
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; squash images towards the center of the frame
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

; paint two painters next to each other
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))