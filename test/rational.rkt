#lang racket

(define (gcd x y)
  (define (gcd-real a b)
    (if (= b 0)
        a
        (gcd-real b (remainder a b))))
  (if (> x y)
      (gcd-real x y)
      (gcd-real y x)))

(define (make-rat nn dd)
  (define (make-rat-real n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (let ((rat-tmp (make-rat-real (abs nn) (abs dd))))
    (if (< (* nn dd) 0)
        (cons (- 0 (car rat-tmp)) (cdr rat-tmp)) 
        (cons (car rat-tmp) (cdr rat-tmp)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (=  (* (numer x) (denom y))
      (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))