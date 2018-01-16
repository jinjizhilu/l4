#lang racket

(define (double f)
  (lambda (x) (f (f x))))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (compose f g)
  (define (newf x) (f (g x)))
  newf)

(define (repeated f n)
  (define (f-iter i s)
    (if (= i 0)
        s
        (f-iter (- i 1) (f s))))
  (lambda (x) (f-iter n x)))

(define (repeated2 f n)
  (if (= n 1)
      f
      (lambda (x)
        (f ((repeated2 f (- n 1)) x)))))

(define (smooth f)
  (let ((dx 0.00000001))
    (lambda (x) (/ (+ (f x)
                      (f (- x dx))
                      (f (+ x dx)))
                   3))))

(define (smooth-n-times f n)
  (define (iter i smooth-f)
    (if (= i 0)
        smooth-f
        (iter (- i 1) (smooth smooth-f))))
  (iter n f))
       