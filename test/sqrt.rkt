#lang racket
(define (sqrt x)
  (define (abs value)
    (if (< value 0)
        (- 0 value)
        value))
  (define (good-enough? guess next-guess)
    (< (abs (/ (- guess next-guess)
               next-guess))
       0.001))
  (define (improve guess)
    (/ (+ guess (/ x guess))
       2))
  (define (sqrt-iter guess)
    (let ((next-guess (improve guess)))
      (if (good-enough? guess next-guess)
          next-guess
          (sqrt-iter next-guess))))
  (sqrt-iter 1.0))

(sqrt 5)

