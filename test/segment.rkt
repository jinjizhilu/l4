#lang racket

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (let ((x-mid (/ (+ (x-point (start-segment s))
                     (x-point (end-segment s)))
                  2.0))
        (y-mid (/ (+ (y-point (start-segment s))
                    (y-point (end-segment s)))
                 2.0)))
  (print-point (make-point x-mid y-mid))))

(define (segment-length s)
  (let ((dx (abs (- (x-point (start-segment s))
                   (x-point (end-segment s)))))
        (dy (abs (- (y-point (start-segment s))
                   (y-point (end-segment s))))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (make-square s1 s2)
  (cons s1 s2))

(define (square-area s)
  (* (car s) (cdr s)))

(define (square-circumference s)
  (* (+ (car s) (cdr s)) 2))