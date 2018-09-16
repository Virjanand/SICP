#lang sicp

(define (pascalTriangle row column)
  (if (= row 1)
      1
      (if (or (= column 1)
              (= column row))
          1
          (+ (pascalTriangle (- row 1)
                             (- column 1))
             (pascalTriangle (- row 1)
                             (+ column 1))))))