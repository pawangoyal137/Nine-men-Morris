#lang racket
(require "eval.rkt")
(require "board.rkt")
(require "list-comprehension.rkt")
(require "maxmin.rkt")
(provide (all-defined-out))
(define board%
  (class object%
    (init-field board)
    (init-field [remove-piece #f])

    (super-new)

    (define/public (make state)
      (cond[(equal? state 'true) (set! remove-piece #t)]
           [(equal? state 'false) (set! remove-piece #f)]
           [else "invalid input"]))))
                                      
                                                                                                        
(define (2d-vector-copy v)
  (define vec (make-vector (vector-length v)  #f))
  (define (helper iter)
    (cond [(not (= iter (vector-length v))) (vector-set! vec iter (vector-copy (vector-ref v iter)))
                                            (helper (+ iter 1))]))
  (helper 0)
  vec)

(define (new-piece-board b s r c char) ; puts new piece on board
  (cond [(not (and (or (= s 0) (= s 1) (= s 2)) (or (= r 0) (= r 1) (= r 2)) (or (= c 0) (= c 1) (= c 2))))  "invalid move"]
        [(and ( = r 1) (= c 1))  "invalid move"]
        [(not (equal? 0 (get-piece b s r c)))  "invalid move"]
        [else (let ( [x (convert3->2 s r c)]
                     [w (2d-vector-copy b)]) (set-2d-vector! w (car x) (cdr x) char)
                w)]))


(define (remove-piece-board b s r c ) ; removes piece on board
  (cond [(not ( and (or (= s 0) (= s 1) (= s 2)) (or (= r 0) (= r 1) (= r 2)) (or (= c 0) (= c 1) (= c 2)))) "invalid move"]
        [(and ( = r 1) (= c 1)) "invalid move"]
        [(equal? 0 (get-piece b s r c)) "invalid move"]
        [else (let ( [x (convert3->2 s r c)]
                     [w (2d-vector-copy b)]) (set-2d-vector! w (car x) (cdr x) 0)
                w)]))

;(define (move-piece-board b si ri ci sf rf cf )
;  (cond [(or (equal? 0 (get-piece b si ri ci)) (not (equal? 0 (get-piece b sf rf cf))) (equal? (get-piece b si ri ci) "invalid pos") (equal? (get-piece b sf rf cf) "invalid pos"))
;         "invalid move"]
;        [else (let( [w (2d-vector-copy b)])
;                (displayln "foo")
;                (new-piece w sf rf cf (get-piece b si ri ci))
;                (remove-piece w si ri ci)
;                w)]))
; pawans original move-piece

(define (move-piece-board b si ri ci sf rf cf )
  (cond [(or (equal? 0 (get-piece b si ri ci)) (not (equal? 0 (get-piece b sf rf cf))) (equal? (get-piece b si ri ci) "invalid pos") (equal? (get-piece b sf rf cf) "invalid pos"))
         "invalid move"]
        [else (let( [w (2d-vector-copy b)])
                ;(displayln "foo")
                (move-piece w si ri ci sf rf cf)
                w)]))

(define (lop b char) ;list of pieces
  (define acc '() )
 (define s 0)
  (define r 0)
  (define c 0)
  (define (nextc ) (set! c (modulo ( + c 1) 3)))
  (define (nextr ) (set! r (modulo ( + r (quotient (+ c 1) 3)) 3)))
  (define (nexts ) (set! s (modulo (+ s (quotient (+ r (quotient (+ c 1) 3)) 3)) 3)))
  (define (iter)
    (cond ( (and ( = s 2) (= r 2) (= c 2)) (cond ( (equal? (get-piece b s r c)  char) ;(equal? (get-piece b s r c)  'b))
                                             (set! acc (cons (list s r c) acc ))) 
                                               (else (void)  )))
          (  (and (= r 1) (= c 1))  (nexts) (nextr) (nextc) (iter))
          ( else 
                 (cond ( (equal? (get-piece b s r c)  char) ;(equal? (get-piece b s r c)  'b))
                                             (set! acc (cons (list s r c )  acc )) (nexts) (nextr) (nextc) (iter) )
                                                 (else (nexts) (nextr) (nextc) (iter))))))
  (begin (iter) acc))
   
;(define (all-possible-boards board-object char count)
;  (let ([board (get-field board board-object)])
;    (cond [(equal? (get-field remove-piece board-object) #t) (map (lambda (x) (let ([new-board-object (make-object board% x)])
;                                                                                (send new-board-object make 'false)     
;                                                                                new-board-object)) (remove board char))]
;          [(not (= count 0)) (map (lambda (x) (let ([new-board-object (make-object board% (cdr x))])
;                                                (cond[(car x) (send new-board-object make 'true)])     
;                                                new-board-object)) (put-move board char))]
;          [(and (= count 0) (> (num-of board char) 3)) (map (lambda (x) (let ([new-board-object (make-object board% (cdr x))])
;                                                                            (cond[(car x) (send new-board-object make 'true)])     
;                                                                            new-board-object)) (move-1 board char))]
;          [(and (= count 0) (= (num-of board char) 3))](map (lambda (x) (let ([new-board-object (make-object board% (cdr x))])
;                                                                            (cond[(car x) (send new-board-object make 'true)])     
;                                                                            new-board-object)) (move-random board char)))))

(define (all-possible-boards board-object char count)
  (let ([board (get-field board board-object)])
    (cond [(equal? (get-field remove-piece board-object) #t) (map (lambda (x) (let ([new-board-object (make-object board% x)])
                                                                                (send new-board-object make 'false)     
                                                                                new-board-object)) (remove board char))]
          [(not (= count 0)) (map (lambda (x) (let ([new-board-object (make-object board% (cdr x))])
                                                (cond[(car x) (send new-board-object make 'true)])     
                                                new-board-object)) (put-move board char))]
          [(and (= count 0) (> (num-of board char) 3)) (map (lambda (x) (let ([new-board-object (make-object board% (cdr x))])
                                                                            (cond[(car x) (send new-board-object make 'true)])     
                                                                            new-board-object)) (move-1 board char))]
          [(and (= count 0) (= (num-of board char) 3))(map (lambda (x) (let ([new-board-object (make-object board% (cdr x))])
                                                                            (cond[(car x) (send new-board-object make 'true)])     
                                                                            new-board-object)) (move-random board char))]
          [else '()])))

(define (remove board char)
  (let* ([all-pieces (lop board (if (equal? char 'w) 'b 'w))]
         [not-part-of-closed-mill (filter (lambda (x) (not (apply closed-mill? (append (list board) x)))) all-pieces)]
         [board-list (map (lambda (x) (apply remove-piece-board (append (list board) x))) not-part-of-closed-mill )])
   (cond [(null? board-list) (map (lambda (x) (apply remove-piece-board (append (list board) x))) all-pieces)]
         [else board-list])))

(define (move-1 board char)
  (let* ([all-pieces (lop board char)]
         [neighbour-piece-pair (map (lambda (x) (cons x (apply neighbours x))) all-pieces)]
         [empty-neighbour (map (lambda (x) (cons (car x) (filter (lambda (y) (equal? (apply get-piece (append (list board) y)) 0)) (cdr x)))) neighbour-piece-pair)]
         [list-neghbour-point (append* (map (lambda (x) (map (lambda (y) (cons (car x)  y)) (cdr x))) empty-neighbour))]
         [finalpos-board-pair (map (lambda (x) (cons (cdr x) (apply move-piece-board (append (list board) (car x) (cdr x))))) list-neghbour-point)]
         [with-closed-mill-formed (map (lambda (x) (cons (apply closed-mill? (append (list (cdr x)) (car x))) (cdr x))) finalpos-board-pair)])
    with-closed-mill-formed))

(define (move-random board char)
  (let* ([all-pieces (lop board char)]
         [empty-space (lop board 0)]
         [finalpos-board-pair (lc (cons y (apply move-piece-board (append (list board) x y))) : x <- all-pieces y <- empty-space)]
         [with-closed-mill-formed (map (lambda (x) (cons (apply closed-mill? (append (list (cdr x)) (car x))) (cdr x))) finalpos-board-pair)])
    with-closed-mill-formed))
        

(define (put-move board char)
  (let* ([all-possible (lc (list a b c) : a <- '(0 1 2) b <- '(0 1 2) c <- '(0 1 2))]
         [improved-list (map (lambda (x) (cons x (cons board (append x (list char))))) all-possible)]
         [true-boards (filter (lambda (x) (vector? (cdr x))) (map (lambda(x) (cons (car x) (apply new-piece-board (cdr x)))) improved-list))]
         [with-closed-mill-formed (map (lambda (x) (cons (apply closed-mill? (append (list (cdr x)) (car x))) (cdr x))) true-boards)])
    with-closed-mill-formed))

(define (generate-tree board-object counter char depth distance-from-top) ;generates tree for a given board-object
  (let ([value ((if (not (= counter 0)) eval-startgame
                    eval-midendgame)
                (get-field board board-object))])
    (cond [(= depth 0) (gnode (- value (* 800 distance-from-top))'())]
          [else (let* ([moves (all-possible-boards board-object char counter)]
                      [next-remove (filter (lambda (x) (equal? #t (get-field remove-piece x))) moves)]
                      [next-not-remove (filter (lambda (x) (not (equal? #t (get-field remove-piece x)))) moves)]
                      [list-remove (append* (map (lambda (x) (all-possible-boards x char (- counter 1))) next-remove))]
                      [total-moves (append list-remove next-not-remove)]
                      [new-counter (cond [ (= counter 0) counter]
                                         [(not (= counter 0)) (- counter 1)])])
                  
                  (cond[(or (= value 10000) (= value -10000) (null? moves)) (gnode value '())]
                       [else (gnode 'no-value-required (map (lambda (x) (apply generate-tree (append (list x) (list new-counter (nott char)
                                                                                                                    (- depth 1) (+ 1 distance-from-top))))) total-moves))]))])))

(define (next-move-helper board global-counter) ;return board and not object
  (define depth 3)
  (define board-object (make-object board% board))
  (define total-moves (let* ([moves (all-possible-boards board-object 'w global-counter)]
                             [partition-moves (call-with-values (lambda () (partition (lambda (x) (equal? #t (get-field remove-piece x))) moves)) list)]
                             [next-remove (car partition-moves)]
                             [list-remove (append* (map (lambda (x) (all-possible-boards x 'w (- global-counter 1))) next-remove))])
                        (append list-remove (cadr partition-moves))))
  
  (define tree-board-list (map (lambda (x y) (cons (maxmin y #f -inf.0 +inf.0) x))
                               total-moves
                               (map (lambda (x) (let ([new-counter (cond [(= global-counter 0) global-counter]
                                                                         [(not (= global-counter 0)) (- global-counter 1)])])
                                                  (apply generate-tree (append (list x) (list new-counter 'b  (- depth 1) 1))))) total-moves)))
 
  (get-field board (cdr (argmax car tree-board-list))))
                                                                                                                    
       
  (define (nott char)
    (if (equal? char 'b) 'w
        'b))

(define (end? w)
  (if (or (not (= 0 pieces-leftc)) (not (= 0 pieces-leftu)))
      #f
      (cond ((< (num-of w 'w) 3) #t)
            ((= (eval-blocked-self w) -10000) #t)
            ((< (num-of w 'b) 3) #t)
            ((= (eval-blocked-opp w) 10000) #t)
            (else #f))))

(define (next-move board global-counter)
  (if (end? board)
      "end-it"
      (next-move-helper board global-counter)))
  

;(define x (2d-vector 7 7))
;(new-piece x 0 0 0 'w)
;(new-piece x 0 2 0 'w)
;(new-piece x 1 0 1 'b)
;(new-piece x 2 0 0 'b)
;(new-piece x 2 0 1 'w)
;(new-piece x 2 0 2 'b)
;(print-board x)
  
  




    

  