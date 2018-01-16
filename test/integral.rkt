#lang racket
(define (cube x)
  (* x x x))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))

(define (sum2 term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (let ((h (/ (* (- b a) 1.0) n)))
    (define (fx i)
      (let ((y (f (+ a (* h i)))))
        (cond ((or (= i 0) (= i n))
               y)
              ((= (remainder i 2) 0)
               (* 2 y))
              (else (* 4 y)))))
    (define (inc x) (+ x 1))
    (* (sum fx 0 inc n)
       (/ h 3.0))))
