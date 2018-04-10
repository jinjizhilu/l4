#lang racket
(define (iter x)
  (if (> x 10000)
      x
      (iter (+ 1 x))))

(iter 0)