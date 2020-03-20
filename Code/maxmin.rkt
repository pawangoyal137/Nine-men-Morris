#lang racket

(provide (all-defined-out))
         
(struct gnode (val list) #:transparent)

(define (maxmin tree isMaximisingPlayer alpha beta)
  (cond[(null? (gnode-list tree)) (gnode-val tree)]
       [isMaximisingPlayer (define best-value -inf.0)
                           (define (helper iter)
                             (if (>= iter (length (gnode-list tree))) best-value
                                 (let ([val (maxmin (list-ref (gnode-list tree) iter) #f alpha beta)])
                                   (begin
                                     (set! best-value (max best-value val))
                                     (set! alpha (max alpha best-value))
                                     (if (>= alpha beta) best-value
                                         (helper (+ iter 1)))))))
                           (helper 0)]
       [else (define best-value +inf.0)
             (define (helper iter)
               (if (>= iter (length (gnode-list tree))) best-value
                   (let ([val (maxmin (list-ref (gnode-list tree) iter) #t alpha beta)])
                     (begin
                       (set! best-value (min best-value val))
                       (set! beta (min alpha best-value))
                       (if (>= alpha beta) best-value
                           (helper (+ iter 1)))))))
             (helper 0)]))


(define t2 (gnode 3 '()))
(define t3 (gnode 5 '()))
(define t4 (gnode 6 '()))
(define t5 (gnode 9 '()))
(define t6 (gnode 1 '()))
(define t7 (gnode 2 '()))
(define t8 (gnode 0 '()))
(define t9 (gnode -1 '()))
(define t10 (gnode 'D (list t2 t3)))
(define t11 (gnode 6 '()))
(define t12 (gnode 'F (list t6 t7)))
(define t13 (gnode 'G (list t8 t9)))
(define t14 (gnode 'B (list t10 t11)))
(define t15 (gnode 'C (list t12 t13)))
(define t1 (gnode 'A (list t14 t15)))
                               