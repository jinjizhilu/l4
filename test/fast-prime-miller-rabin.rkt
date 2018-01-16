#lang racket

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((= (remainder exp 2) 0)
         (let ((r (expmod base (/ exp 2) m)))
           (let ((rr (remainder (square r) m)))
             (if (or (= r (- m 1)) (= r 1) (> rr 1))
                 rr
                 0))))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))