#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1)) 

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ (if (pair? x)
                       (count-leaves x)
                       1)
                   y))
              0 t))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map first seqs))
            (accumulate-n op init (map last seqs)))))

(accumulate add 0 (list 1 2 3 4 5 6 7 8 9))
(accumulate add 0 (cons 9 (cons 8 null)))
(accumulate-n add 0 (list (list 1 2 3 4 5 6 7 8 9) (list 2 3 4 5 6 7 8 9 10)))