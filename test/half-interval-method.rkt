#lang racket
(define (search f negpoint pospoint)
  (let ((midpoint (/ (+ negpoint pospoint) 2)))
    (if (< (abs (- negpoint pospoint)) 0.0001)
           midpoint
          (let ((midvalue (f midpoint)))
            (cond ((> midvalue 0)
                   (search f negpoint midpoint))
                  ((< midvalue 0)
                   (search f midpoint pospoint))
                  (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0))
           (search f a b))
          ((and (> a-value 0) (< b-value 0))
           (search f b a))
          (else
           (display "Values are not of opposite sign" a b)))))

(half-interval-method (lambda (x) (+ (* x x) (* x -2) -1)) -10 2)