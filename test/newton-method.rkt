#lang racket
(define (fix-point f first-guess)
  (let ((next-guess (f first-guess)))
    (if (< (abs (- next-guess first-guess)) 0.00001)
        next-guess
        (fix-point f next-guess))))

(define (deriv g)
  (let ((dx 0.0000001))
    (lambda (x)
      (/ (- (g (+ x dx))
            (g x))
         dx))))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newton-method g guess)
  (fix-point (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (* y y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fix-point (transform g) guess))

(define (sqrt2 x)
  (fixed-point-of-transform (lambda (y) (- (* y y) x))
                            newton-transform
                            1.0))

(define (sqrt3 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            (lambda (g) (lambda (x) (/ (+ (g x) x) 2)))
                            1.0))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
