
#lang racket/gui
(require 2htdp/image)
(require 2htdp/universe)
(require racket/mpair )
(require lang/posn)
(provide (all-defined-out))
(require "board.rkt")
(require "board_gui.rkt")
(require "eval.rkt")
;(require (prefix-in main: "main_gui.rkt"))
(define a 700)
(require "lplay.rkt")
(displayln pieces-leftu)
;(provde (all-defined-out))
;(struct posn (x y) #:transparent )
(define (get-coords s r c a) ; return a make -posn pair
  (make-posn (+ (* 0.5 a) (* (- c 1) a (+ 0.1 (* 0.15 (- 2 s))))) (+ (* 0.5 a) (* (- r 1) a (+ 0.1 (* 0.15 (- 2 s))))) ))
; we represent current state (or world) as our board 7X7 vector
(define (all-on-board b)
  (define acc '() )
 (define s 0)
  (define r 0)
  (define c 0)
  (define (nextc ) (set! c (modulo ( + c 1) 3)))
  (define (nextr ) (set! r (modulo ( + r (quotient (+ c 1) 3)) 3)))
  (define (nexts ) (set! s (modulo (+ s (quotient (+ r (quotient (+ c 1) 3)) 3)) 3)))
  (define (iter)
    (cond ( (and ( = s 2) (= r 2) (= c 2)) (cond ( (equal? (get-piece b s r c)  'w) ;(equal? (get-piece b s r c)  'b))
                                             (set! acc (cons (make-object gamepiecec%  (get-coords s r c a) s r c) acc )) )
                                                 ( (equal? (get-piece b s r c)  'b) ;(equal? (get-piece b s r c)  'b))
                                             (set! acc (cons (make-object gamepieceu%  (get-coords s r c a) s r c) acc )) )))
          ( (or (and (= r 1) (= c 1)) (equal? (get-piece b s r c) 0)) (nexts) (nextr) (nextc) (iter))
          ( else 
                 (cond ( (equal? (get-piece b s r c)  'w) ;(equal? (get-piece b s r c)  'b))
                                             (set! acc (cons (make-object gamepiecec%  (get-coords s r c a) s r c) acc )) )
                                                 ( (equal? (get-piece b s r c)  'b) ;(equal? (get-piece b s r c)  'b))
                                             (set! acc (cons (make-object gamepieceu%  (get-coords s r c a) s r c) acc )) ))
                 (nexts) (nextr) (nextc) (iter))))
  (begin (iter) acc))


;(define (draw w) ; w - current world
 ; ( place-images (list
(define b (2d-vector 7 7)) ;; changed here
;(new-piece b 0 2 1 'w)
;;(new-piece b 2  2 2 'b)
;(new-piece b 0 0 1 'w)
;(new-piece b 0 0 2 'w)
;(new-piece b 0 0 0 'w)
(define tbm #f) ;tbm - to be moved
(define (my-list-updater l f up) ; f used to search for equality and deos up on element l 
  (list-update l (index-where l f) up)) 
(define (drawer w)  
  (define pieces (all-on-board w)) ; list of gamepiece objects
  (define color (map (lambda (t) (get-field img t)) pieces)) ; colorm is a list of shape as images
  (define pos (map (lambda (t) (get-field pos t)) pieces ))
  (define t (text (string-append "pieces-remaining: " (number->string pieces-leftu )) 30 "black"))
  (define tw (text (string-append "pieces-remaining: " (number->string pieces-leftc )) 30 "white"))
  (define remt  (text "     you closed a mill !
remove an opponent's piece" 15 "black")  )
  (define remtw  (text "     you closed a mill !
remove an opponent's piece" 15 "white")  )
  (cond ((equal? state '0) hs)
    (  (equal? state '1r) (place-images (append (list remt t)  color ) (append (list (make-posn (* 0.14 a) (* 0.03 a))  (make-posn (* 0.7 a) (* 0.02 a)))  pos) bg))
    (  (equal? state '1rw) (place-images (append (list remtw t)  color ) (append (list (make-posn (* 0.14 a) (* 0.03 a))  (make-posn (* 0.7 a) (* 0.02 a)))  pos) bg))
        ( (or (equal? state '3r) (equal? state '2r)) (place-images (cons remt  color ) (cons (make-posn (* 0.14 a) (* 0.03 a))  pos) bg))
        ( (or (equal? state '3rw) (equal? state '2rw)) (place-images (cons remtw  color ) (cons (make-posn (* 0.14 a) (* 0.03 a))  pos) bg))
        ( (or (equal? state '3mw) (equal? state '2mw))
          (place-images ( cons (circle 40 "solid" "white") color) (cons (get-field pos tbm) pos) bg))
        ( (or (equal? state '3m) (equal? state '2m))
          (place-images ( cons (circle 40 "solid" "black") color) (cons (get-field pos tbm) pos) bg))
  ((equal? state '1p) (place-images (cons  t  color ) (cons (make-posn (* 0.7 a) (* 0.02 a))  pos) bg))
  ((equal? state '1pw) (place-images (cons  tw  color ) (cons (make-posn (* 0.7 a) (* 0.02 a))  pos) bg))
  (else (place-images color pos bg) ))) ; 2p & 3p comes here , 2pw 3pw also
; extra msg in state '1r two texts appended to list.
(define state '1p )

(define (f0 w x y me)
  (cond ((and (equal? me "button-down") (<= (magnitude (- x (/ a 2))) 70) (<= (magnitude (- y (/ a 2))) 40 ))  (begin (set! state '1p) w))
        (else (begin (displayln y) w))))
  

(define ( f1p w x y me)
  (define reqd_button (findf (lambda (t) (send t over-it? x y )) buttonlist))
  (if (or (not (equal? me "button-down"))(equal? reqd_button #f)) w
   (begin (let ([ x1 (new-piece w (get-field s reqd_button ) (get-field r reqd_button ) (get-field c reqd_button ) 'b)])
            
            ;(set! pieces (all-on-board w))
          (cond ((and (not (string? x1))  (closed-mill? w (get-field s reqd_button ) (get-field r reqd_button ) (get-field c reqd_button ) )) (set! state '1r))
            ( (and (not (string? x1)) ( equal? 0 pieces-leftc))  (display "a")  (set! state '2pw))
            ((not (string? x1))  (display "b") (set! state '1pw))
            (else w))
   w ))))

(define ( f1pw w x y me)
  (define reqd_button (findf (lambda (t) (send t over-it? x y )) buttonlist))
  (if (or (not (equal? me "button-down"))(equal? reqd_button #f)) w
   (begin (let ([ x1 (new-piece w (get-field s reqd_button ) (get-field r reqd_button ) (get-field c reqd_button ) 'w)])
            (cond ((not (string? x1)) (decc)))
            ;(set! pieces (all-on-board w))
          (cond ((and (not (string? x1))  (closed-mill? w (get-field s reqd_button ) (get-field r reqd_button ) (get-field c reqd_button ) ))  (set! state '1rw))
            ( (and (not (string? x1)) ( equal? 0 pieces-leftc))    (set! state '2p))
            ((not (string? x1))  (display "b") (set! state '1p) )
            (else w))
   w ))))
(define ce3 #f) ; computer that is white entererd three, not really computer just the human reperesenting white piece
(define ue3 #f) ; user that is black entrered three
(define (f2p w x y me)  
  (define pieces (all-on-board w))
  (cond ((= 3 (length (filter (lambda (t) (equal? "black" (get-field color t))) pieces))) (set! ue3 #t) (set! state '3p) w))
  (define to_be_moved (findf (lambda (t) (and (is-a? t gamepieceu% ) (send t over-it? x y ))) pieces))
  (cond ( (equal? to_be_moved #f) w)
        ( (equal? me "button-down")  (set! tbm to_be_moved ) (set! state '2m) w)
        (else w)))

(define (f2pw w x y me)  
  (define pieces (all-on-board w))
  (cond ((= 3 (length (filter (lambda (t) (equal? "white" (get-field color t))) pieces))) (set! ce3 #t) (set! state '3pw) w))
  (define to_be_moved (findf (lambda (t) (and (is-a? t gamepiecec% ) (send t over-it? x y ))) pieces))
  (cond ( (equal? to_be_moved #f) w)
        ( (equal? me "button-down")  (set! tbm to_be_moved ) (set! state '2mw) w)
        (else w)))



(define (f2m w x y me)
  (define pieces (all-on-board w))
  (define nl (findf (lambda (t) (send t over-it? x y )) buttonlist)) ;new location
  (define black-pressed (findf (lambda (t) (send t over-it? x y)) (filter (lambda (t) (is-a? t gamepieceu%)) pieces)))
 ;(cond ((and (equal? me "button-down") (not (equal? nl #f))) (displayln (get-field s nl)) (displayln (get-field r nl)) (displayln (get-field c nl))))
  (cond ((and (equal? me "button-down")  (not (equal? black-pressed #f)))  
              (set! tbm black-pressed) w)

    ((and (equal? me "button-down") (or  (equal? #f nl) (equal? (get-field pos tbm) (get-field pos nl))
                 (not (equal? 0 (get-piece w (get-field s nl) (get-field r nl) (get-field c nl) ))))) (set! state '2p) (set! tbm #f) w )
        ( (and (equal? me "button-down") (not (equal? #f (index-of (neighbours (get-field s nl) (get-field r nl) (get-field c nl)) (list  (get-field s tbm) (get-field r tbm) (get-field c tbm)))))
                )
        (begin (print-board w)
            (move-piece w (get-field s tbm) (get-field r tbm) (get-field c tbm) (get-field s nl) (get-field r nl) (get-field c nl))
                         (if (closed-mill? w (get-field s nl) (get-field r nl) (get-field c nl)) (set! state '2r)
                             (begin (if ce3 (set! state '3pw) (set! state '2pw) )  ))
           (print-board w)     w))
        (else w)) ; if u click on a circle that is a red one but not a neighbour
  )

(define (f2mw w x y me)
  (define pieces (all-on-board w))
  (define nl (findf (lambda (t) (send t over-it? x y )) buttonlist)) ;new location
  (define white-pressed (findf (lambda (t) (send t over-it? x y)) (filter (lambda (t) (is-a? t gamepiecec%)) pieces)))
 ;(cond ((and (equal? me "button-down") (not (equal? nl #f))) (displayln (get-field s nl)) (displayln (get-field r nl)) (displayln (get-field c nl))))
  (cond ((and (equal? me "button-down")  (not (equal? white-pressed #f)))  
              (set! tbm white-pressed) w)

    ((and (equal? me "button-down") (or  (equal? #f nl) (equal? (get-field pos tbm) (get-field pos nl))
                 (not (equal? 0 (get-piece w (get-field s nl) (get-field r nl) (get-field c nl) ))))) (set! state '2pw) (set! tbm #f) w )
        ( (and (equal? me "button-down") (not (equal? #f (index-of (neighbours (get-field s nl) (get-field r nl) (get-field c nl)) (list  (get-field s tbm) (get-field r tbm) (get-field c tbm)))))
                ) (begin (print-board w) (move-piece w (get-field s tbm) (get-field r tbm) (get-field c tbm) (get-field s nl) (get-field r nl) (get-field c nl))
                         (if (closed-mill? w (get-field s nl) (get-field r nl) (get-field c nl)) (set! state '2rw)
                             (begin (if ue3 (set! state '3p) (set! state '2p) )   ))
           (print-board w)     w))
        (else w)) ; if u click on a circle that is a red one but not a neighbour
  )

(define (f2r w x y me)
  (define pieces (all-on-board w))
  (define to_be_removed (findf (lambda (t) (send t over-it? x y )) pieces))
  (define ns (if ce3 '3pw '2pw))
  
 ; (cond ( ( equal? 0 pieces-leftu) (set! ns '2p)))
  (cond ( (and (is-a? to_be_removed gamepiecec% ) (equal? me "button-down") (not (all-in-closed w pieces))
          (not (closed-mill? w (get-field s to_be_removed ) (get-field r to_be_removed ) (get-field c to_be_removed )))) (begin (remove-piece w (get-field s to_be_removed )
                                                                                                                                         (get-field r to_be_removed )
                                                                                                                                         (get-field c to_be_removed ))
                                                                                                                                  
                                                                                                                               (set! state ns) w)  )
        ( (and (is-a? to_be_removed gamepiecec% ) (equal? me "button-down") (all-in-closed w pieces))
          (begin (remove-piece w (get-field s to_be_removed )   (get-field r to_be_removed )
                                                                (get-field c to_be_removed ))
                   
                                                                (set! state ns) w))
          
          (else w)))

(define (f2rw w x y me)
  (define pieces (all-on-board w))
  (define to_be_removed (findf (lambda (t) (send t over-it? x y )) pieces))
  (define ns (if ue3 '3p '2p))
 ; (cond ( ( equal? 0 pieces-leftu) (set! ns '2p)))
  (cond ( (and (is-a? to_be_removed gamepieceu% ) (equal? me "button-down") (not (all-in-closed-black w pieces))
          (not (closed-mill? w (get-field s to_be_removed ) (get-field r to_be_removed ) (get-field c to_be_removed ))))
          (begin (remove-piece w (get-field s to_be_removed )
                               (get-field r to_be_removed )
                               (get-field c to_be_removed ))
                 
                 (set! state ns) w)  )
        ( (and (is-a? to_be_removed gamepieceu% ) (equal? me "button-down") (all-in-closed-black w pieces))
          (begin (remove-piece w (get-field s to_be_removed )
                               (get-field r to_be_removed )
                                 (get-field c to_be_removed ))
                   
                                                                (set! state ns) w))
          
          (else w)))


(define (f1r w x y me)
  (define pieces (all-on-board w))
  (define to_be_removed (findf (lambda (t) (send t over-it? x y )) pieces))
  (define ns '1pw)
  (cond ( ( equal? 0 pieces-leftc) (set! ns '2pw)))
  (cond ( (and (is-a? to_be_removed gamepiecec% ) (equal? me "button-down") (not (all-in-closed w pieces))
          (not (closed-mill? w (get-field s to_be_removed ) (get-field r to_be_removed ) (get-field c to_be_removed )))) (begin (remove-piece w (get-field s to_be_removed )
                                                                                                                                         (get-field r to_be_removed )
                                                                                                                                         (get-field c to_be_removed ))
                                                                                                                                 
                                                                                                                               (set! state ns) w)  )
        ( (and (is-a? to_be_removed gamepiecec% ) (equal? me "button-down") (all-in-closed w pieces))
          (begin (remove-piece w (get-field s to_be_removed )   (get-field r to_be_removed )
                                                                (get-field c to_be_removed ))  
                                                                (set! state ns) w))
          
          (else w)))

(define (f1rw w x y me)
  (define pieces (all-on-board w))
  (define to_be_removed (findf (lambda (t) (send t over-it? x y )) pieces))
  (define ns '1p)
  (cond ( ( equal? 0 pieces-leftc) (set! ns '2p)))
  (cond ( (and (is-a? to_be_removed gamepieceu% ) (equal? me "button-down") (not (all-in-closed-black w pieces))
          (not (closed-mill? w (get-field s to_be_removed ) (get-field r to_be_removed ) (get-field c to_be_removed )))) (begin (remove-piece w (get-field s to_be_removed )
                                                                                                                                         (get-field r to_be_removed )
                                                                                                                                         (get-field c to_be_removed ))
                                                                                                                                 
                                                                                                                               (set! state ns) w)  )
        ( (and (is-a? to_be_removed gamepieceu% ) (equal? me "button-down") (all-in-closed-black w pieces))
          (begin (remove-piece w (get-field s to_be_removed )   (get-field r to_be_removed )
                                                                (get-field c to_be_removed ))  
                                                                (set! state ns) w))
          
          (else w)))

(define (f3p w x y me)  
  (define pieces (all-on-board w))
  ;(cond ((= 3 (length (filter (lambda (t) (equal? "black" (get-field color t))) pieces))) (set! state '3p) w))
  (define to_be_moved (findf (lambda (t) (and (is-a? t gamepieceu% ) (send t over-it? x y ))) pieces))
  (cond ( (equal? to_be_moved #f) w)
        ( (equal? me "button-down")  (set! tbm to_be_moved ) (set! state '3m) w)
        (else w)))

(define (f3pw w x y me)  
  (define pieces (all-on-board w))
  ;(cond ((= 3 (length (filter (lambda (t) (equal? "white" (get-field color t))) pieces))) (set! state '3p) w))
  (define to_be_moved (findf (lambda (t) (and (is-a? t gamepiecec% ) (send t over-it? x y ))) pieces))
  (cond ( (equal? to_be_moved #f) w)
        ( (equal? me "button-down")  (set! tbm to_be_moved ) (set! state '3mw) w)
        (else w)))

(define (f3m w x y me)
  (define pieces (all-on-board w))
  (define nl (findf (lambda (t) (send t over-it? x y )) buttonlist)) ;new location
  (define black-pressed (findf (lambda (t) (send t over-it? x y)) (filter (lambda (t) (is-a? t gamepieceu%)) pieces)))
 ;(cond ((and (equal? me "button-down") (not (equal? nl #f))) (displayln (get-field s nl)) (displayln (get-field r nl)) (displayln (get-field c nl))))
  (cond ((and (equal? me "button-down")  (not (equal? black-pressed #f)))  
              (set! tbm black-pressed) w)

    ((and (equal? me "button-down") (or  (equal? #f nl) (equal? (get-field pos tbm) (get-field pos nl))
                 (not (equal? 0 (get-piece w (get-field s nl) (get-field r nl) (get-field c nl) ))))) (set! state '3p) (set! tbm #f) w )
        (  (equal? me "button-down") 
                 (begin (print-board w) (move-piece w (get-field s tbm) (get-field r tbm) (get-field c tbm) (get-field s nl) (get-field r nl) (get-field c nl))
                         (if (closed-mill? w (get-field s nl) (get-field r nl) (get-field c nl)) (set! state '3r)  (begin (if ce3 (set! state '3pw) (set! state '2pw))))
           (print-board w)     w))
        (else w)) ; if u click on a circle that is a red one but not a neighbour
  )

(define (f3mw w x y me)
  (define pieces (all-on-board w))
  (define nl (findf (lambda (t) (send t over-it? x y )) buttonlist)) ;new location
 (define white-pressed (findf (lambda (t) (send t over-it? x y)) (filter (lambda (t) (is-a? t gamepiecec%)) pieces)))
 ;(cond ((and (equal? me "button-down") (not (equal? nl #f))) (displayln (get-field s nl)) (displayln (get-field r nl)) (displayln (get-field c nl))))
  (cond ((and (equal? me "button-down")  (not (equal? white-pressed #f)))  
              (set! tbm white-pressed) w)

    ((and (equal? me "button-down") (or  (equal? #f nl) (equal? (get-field pos tbm) (get-field pos nl))
                 (not (equal? 0 (get-piece w (get-field s nl) (get-field r nl) (get-field c nl) ))))) (set! state '3pw) (set! tbm #f) w )
        (  (equal? me "button-down") 
                 (begin (print-board w) (move-piece w (get-field s tbm) (get-field r tbm) (get-field c tbm) (get-field s nl) (get-field r nl) (get-field c nl))
                         (if (closed-mill? w (get-field s nl) (get-field r nl) (get-field c nl)) (set! state '3rw)   (begin (if ue3 (set! state '3p) (set! state '2p))))
           (print-board w)     w))
        (else w)) ; if u click on a circle that is a red one but not a neighbour
  )

(define (f3r w x y me)
  (define pieces (all-on-board w))
  (define to_be_removed (findf (lambda (t) (send t over-it? x y )) pieces))
  (define ns (if ce3 '3pw '2pw))
 ; (cond ( ( equal? 0 pieces-leftu) (set! ns '3p)))
  (cond ( (and (is-a? to_be_removed gamepiecec% ) (equal? me "button-down") (not (all-in-closed w pieces))
          (not (closed-mill? w (get-field s to_be_removed ) (get-field r to_be_removed ) (get-field c to_be_removed )))) (begin (remove-piece w (get-field s to_be_removed )
                                                                                                                                         (get-field r to_be_removed )
                                                                                                                                         (get-field c to_be_removed ))
                                                                                                                                
                                                                                                                               (set! state ns) w)  )
        ( (and (is-a? to_be_removed gamepiecec% ) (equal? me "button-down") (all-in-closed w pieces))
          (begin (remove-piece w (get-field s to_be_removed )   (get-field r to_be_removed )
                                                                (get-field c to_be_removed ))
                                                                
                                                                (set! state ns) w))
          
          (else w)))

(define (f3rw w x y me)
  (define pieces (all-on-board w))
  (define to_be_removed (findf (lambda (t) (send t over-it? x y )) pieces))
  (define ns (if ue3 '3p '2p))
 ; (cond ( ( equal? 0 pieces-leftu) (set! ns '3p)))
  (cond ( (and (is-a? to_be_removed gamepieceu% ) (equal? me "button-down") (not (all-in-closed-black w pieces))
          (not (closed-mill? w (get-field s to_be_removed ) (get-field r to_be_removed ) (get-field c to_be_removed )))) (begin (remove-piece w (get-field s to_be_removed )
                                                                                                                                         (get-field r to_be_removed )
                                                                                                                                         (get-field c to_be_removed ))
                                                                                                                                
                                                                                                                               (set! state ns) w)  )
        ( (and (is-a? to_be_removed gamepieceu% ) (equal? me "button-down") (all-in-closed-black w pieces))
          (begin (remove-piece w (get-field s to_be_removed )   (get-field r to_be_removed )
                                                                (get-field c to_be_removed ))
                                                                
                                                                (set! state ns) w))
          
          (else w)))

(define (all-in-closed b p) ; list of pieces, ie objects
  (andmap (lambda (t) (closed-mill? b (get-field s t ) (get-field r t ) (get-field c t ))) (filter (lambda (t) (equal? "white" (get-field color t))) p )))
(define (all-in-closed-black b p) ; list of pieces, ie objects
  (andmap (lambda (t) (closed-mill? b (get-field s t ) (get-field r t ) (get-field c t ))) (filter (lambda (t) (equal? "black" (get-field color t))) p )))
        
;(displayln pieces-leftc )
(define endstate #f)
(define (endgame? w)
  (if (or (not (= 0 pieces-leftc)) (not (= 0 pieces-leftu)))
      #f
      (cond ((< (num-of w 'w) 3) (begin (set! endstate 'w) #t))
            ((= (eval-blocked-self w) -10000) (begin (set! endstate 'w) #t))
            ((< (num-of w 'b) 3) (begin (set! endstate 'b) #t))
            ((= (eval-blocked-opp w) 10000) (begin (set! endstate 'b) #t))
            (else #f))))
  
(define (mhandler w x y me) (displayln state) (displayln pieces-leftc)
  (cond ((equal? state '0) (f0 w x y me))
    ( (equal? state '1p) (f1p w x y me))
    ( (equal? state '1pw) (f1pw w x y me))
        ( (equal? state '1r) (f1r w x y me))
        ( (equal? state '1rw) (f1rw w x y me))
        ( (equal? state '2p) (f2p w x y me))
        ( (equal? state '2m) (f2m w x y me))
        ( (equal? state '2r) (f2r w x y me))
        ( (equal? state '2pw) (f2pw w x y me))
        ( (equal? state '2mw) (f2mw w x y me))
        ( (equal? state '2rw) (f2rw w x y me))
        ( (equal? state '3p) (f3p w x y me))
        ( (equal? state '3m) (f3m w x y me))
        ( (equal? state '3r) (f3r w x y me))
        ( (equal? state '3pw) (f3pw w x y me))
        ( (equal? state '3mw) (f3mw w x y me))
        ( (equal? state '3rw) (f3rw w x y me))
        (else w)))

;(define bw (place-image

(define (last-scene w)
 
        (cond ((equal? endstate 'b) (place-image (bitmap/file "ww2.png") (/ a 2) (/ a 2) (drawer w)))
        ((equal? endstate 'w) (place-image (bitmap/file "bw2.png") (/ a 2) (/ a 2) (drawer w)))
        (else (drawer w))))

;(big-bang b (to-draw drawer) (on-mouse mhandler ))
(define (p) (big-bang b (to-draw drawer) (on-mouse mhandler ) (stop-when endgame? last-scene )))
;(launch-many-worlds/proc p)
