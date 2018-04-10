#lang racket

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (let ((next-guess (improve guess)))
      (if (good-enough? guess next-guess)
          guess
          (iter next-guess))))
  iter)

(define (good-enough? guess next-guess)
  (< (abs (/ (- guess next-guess)
             next-guess))
     0.001))

(define (sqrt x)
  (define (improve guess)
    (/ (+ guess (/ x guess))
       2))
  ((iterative-improve good-enough? improve x) 1.0))

(define (fix-point f first-guess)
  ((iterative-improve good-enough? f) first-guess))

(define (sqrt2 x)
  (fix-point (lambda (y) (/ (+ (/ x y) y) 2)) 1.0))

(define (calc-theta)
  (fix-point (lambda (y) (+ (/ 1.0 y) 1)) 1.0))

(calc-theta)