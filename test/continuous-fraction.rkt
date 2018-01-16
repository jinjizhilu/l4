#lang racket
(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-iter (+ i 1))))))
  (cont-frac-iter 1))

(define (cont-frac2 n d k)
  (define (cont-frac-iter i sum)
    (if (= i 0)
        sum
        (cont-frac-iter (- i 1) (/ (n i) (+ sum (d i))))))
  (cont-frac-iter k 0))

(define (n x i)
  (if (= i 1)
      x
      (- (* x x))))
  
(define (d i)
  (- (* i 2) 1))

(define (tan-cf x k)
  (cont-frac2 (lambda (i) (n x i)) d k))
              