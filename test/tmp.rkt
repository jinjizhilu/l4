#lang racket
(require racket/trace)

(define (last-pair li)
  (if (null? (cdr li))
      (car li)
      (last-pair (cdr li))))

(define (reverse li)
  (define (reverse-iter lli r)
    (if (null? lli)
        r
        (reverse-iter (cdr lli) (cons (car lli) r))))
  (reverse-iter li (list)))

(define (same-parity . p)
  (let ((parity (remainder (car p) 2)))
    (define (same-parity-iter li)
      (if (null? li)
          (list)
          (if (= parity (remainder (car li) 2))
              (cons (car li) (same-parity-iter (cdr li)))
              (same-parity-iter (cdr li)))))
    (same-parity-iter p)))

(define (square-list items)
  (if (null? items)
      null
      (cons (sqr (car items)) (square-list (cdr items)))))

(define (map func li)
  (if (null? li)
      null
      (cons (func (car li)) (map func (cdr li)))))

(define (deep-reverse li)
  (define (reverse-iter lli r)
    (if (null? lli)
        r
        (let ((first-item (car lli)))
          (let ((first-item-r
                 (if (pair? first-item)
                     (deep-reverse first-item)
                     first-item)))
            (reverse-iter (cdr lli) (cons first-item-r r))))))
  (reverse-iter li null))

(define (fringe t)
  (cond ((null? t) null)
        ((not (pair? t)) (list t))
        (else (append (fringe (car t)) (fringe (cdr t))))))

(define (square-tree t)
  (cond ((null? t) null)
        ((not (pair? t)) (* t t))
        (else (cons (square-tree (car t))
                    (square-tree (cdr t))))))
  
(define (square-tree2 t)
  (map (lambda (st)
         (if (not (pair? st))
             (* st st)
             (square-tree2 st)))
       t))

(define (tree-map func tr)
  (cond ((null? tr) null)
        ((not (pair? tr)) (func tr))
        (else (cons (tree-map func (car tr))
                    (tree-map func (cdr tr))))))

(define (square-tree3 t) (tree-map
                          (lambda (x) (* x x))
                          t))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (x) (cons (car s) x))
                      rest)))))

(trace subsets)