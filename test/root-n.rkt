#lang racket

(define (fast-exp b n)
  (define (fast-exp-iter a c i)
    (cond ((= i 0) a)
          ((= (remainder i 2) 0)
           (fast-exp-iter a (* c c) (/ i 2)))
          (else
           (fast-exp-iter (* a c) c (- i 1)))))
  (fast-exp-iter 1 b n))

(define (fix-point f first-guess)
  (let ((next-guess (f first-guess)))
    (if (< (abs (- next-guess first-guess)) 0.00001)
        next-guess
        (fix-point f next-guess))))

(define (repeated f n)
  (if (= n 1)
      f
      (lambda (x)
        (f ((repeated f (- n 1)) x)))))

(define (average-damp f)
  (lambda (x) (/ (+ (f x) x) 2.0)))

(define (root-n x n)
  (define (f y)
    (/ x (fast-exp y (- n 1))))
  (fix-point ((repeated average-damp 4) f) 1.0))