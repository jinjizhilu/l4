#lang racket
(define (inc x) (+ x 1))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))

(define (accumulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate2 combiner null-value term (next a) next b)
                (term a))))
  
(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n)
  (define (identity x) x)
  (product identity 1 inc n))

(define (calc-pi n)
  (define (term i)
    (if (= (remainder i 2) 1)
        (/ (+ i 1.0) (+ i 2.0))
        (/ (+ i 2.0) (+ i 1.0))))
  (* (product term 1 inc n)
     4.0))