#lang racket
(define (fix-point f first-guess)
  (let ((next-guess (f first-guess)))
    (display next-guess)
    (newline)
    (if (< (abs (- next-guess first-guess)) 0.00001)
        next-guess
        (fix-point f next-guess))))

(define (sqrt x)
  (fix-point (lambda (y) (/ (+ (/ x y) y) 2)) 1.0))

(define (calc-theta)
  (fix-point (lambda (y) (+ (/ 1.0 y) 1)) 1.0))