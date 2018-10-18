#lang sicp

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
        dx)))
(define dx 0.0001)
;Derivative of x^3 at 5
(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;Another form of the square-root procedure:
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                             1.0))

(define (square x)
  (* x x))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt2 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                                        average-damp
                                        1.0))

(define (sqrt3 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                                        newton-transform
                                        1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y)
     2))