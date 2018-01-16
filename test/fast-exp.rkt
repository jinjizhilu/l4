#lang racket
(define (fast-exp b n)
  (define (fast-exp-iter a c i)
    (cond ((= i 0) a)
          ((= (remainder i 2) 0)
           (fast-exp-iter a (* c c) (/ i 2)))
          (else
           (fast-exp-iter (* a c) c (- i 1)))))
  (fast-exp-iter 1 b n))

