#lang racket
(define zero
  (lambda (f) (lambda(x) x)))

(define (add1 n)
  (lambda (f) (lambda(x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (add1 one))

(define (add a b)
  (lambda (f) (lambda(x) ((a f) ((b f) x)))))

(define (inc n)
  (+ n 1))

