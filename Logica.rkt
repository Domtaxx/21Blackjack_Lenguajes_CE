#lang racket 
;(require "Black_Jack_GUI.rkt")
;lista de jugadores((cards-available) ((jugado1 (cartas) state) (jugador2 (cartas) state) ...) )
(provide (all-defined-out))
(define (BCEj1 List)
  (append (list (shuffle(give-deck 51))) (list (BCEj-aux (append '(crupier) List)  '())))
)

(define (BCEj-aux List newList)
  (cond
    ((empty? List) newList)
    (else
     (BCEj-aux
      (cdr List)
      (append newList [list[list(car List) '() #t]])
     )
    )
  )
)
(define (current-player players)
  (car players)
)
(define (current-player-name players)
  (caar players)
)
(define (current-player-cards players)
  (cadar players)
)
(define (other-players players)
  (cdr players)
)
(define (get-player-state players)
  (caddar players)
)
(define (get-deck board-list)
  (car board-list)
)
(define(get-first-card board-list)
  (car (get-deck board-list))
)
(define (get-players board-list)
  (cadr board-list)
)
(define (get-current-player-state board-list)
  (get-player-state (get-players board-list))
)
(define (get-first-player board-list)
  (current-player (get-players board-list))
)
(define (get-other-players board-list)
  (other-players (get-players board-list))
)

(define (hand-out board-list card-num)
  (cond
    ((<= card-num 0) board-list)
    (else
     (hand-out (manage-board-list board-list) (- card-num 1))
    )
  )
)


(define (pass-turn board-list)
  (change-player-list board-list
                      (pass-turn-aux (get-players board-list) #f)
                      )
)
  ;(set-state (get-deck board-list) (get-players board-list) #t)
  

(define (pass-turn-aux players done)
  (cond
    ((= 1 (length players)) players)
    ((not done)
     (pass-turn-aux (append (other-players players) (list (current-player players))) #t)
    )
    ((not (get-player-state players))
     (pass-turn-aux (append (other-players players) (list (current-player players))) #t)
    )
    (else players)
  )
)

(define (change-player-list board-list new-players)
  (append (list (get-deck board-list)) (list new-players))
)

(define (set-state deck player-list state)
  (append
   (list deck)
   (list {append (other-players player-list)
           (list(list
                 (current-player-name player-list)
                 (current-player-cards player-list)
                 state
           ))
          
   }
  ))
)

(define (add-card card player-list)
  {append (list
           (list
            (current-player-name player-list)
            (append (current-player-cards player-list) (list card))
            (get-player-state player-list)
           )
          )
          (other-players player-list)
  }
)

(define (manage-board-list board-list)
  (append (list(cdr(get-deck board-list)))
          (list (add-card (get-first-card board-list) (get-players board-list)))
          )
)




(define (start-game players)
  (set-stay (hand-out-circle (BCEj1 players) (* 2 (+ 1 (length players)))))
)

(define (game-over list-players)
  (cond
    ((empty? list-players)
     #t
    )
    ((equal? (caddar list-players) #t)
     #f
    )
    (else
     (game-over (cdr list-players))
    )
  )
)

(define (give-deck n)
 (cond
   ((negative? n)
    '()
   )
   (else
     (append (list n) (give-deck (- n 1)))
   )
 )
)

(define (set-stay board-list)
  (cond
    ((everyone-done? (get-players board-list))
     (get-scores (get-players board-list))
    )
    (else
     (pass-turn (set-state (get-deck board-list) (get-players board-list) #f))
    )
  )
)

(define (get-scores players)
  (cond
    ((empty? players) '())
    (else
     (append
      (list (list
             (current-player-name players)
             (evaluate-as (add-card-values (current-player-cards players))
                          (char-appearances (current-player-cards players) 0)
             )
            )
      )
      (get-scores (other-players players))
     )
    )
  )
    
)

(define (add-card-values cards)
  (cond
    ((empty? cards) 0)
    (else
     (+ (evaluate-card (car cards)) (add-card-values (cdr cards)))
    )
  )
)

(define (evaluate-card card)
  (cond
    ((zero? (remainder card 13)) 0)
    ((>= (remainder card 13) 9) 10)
    (else (+ (remainder card 13) 1))
  )
)

(define (evaluate-as middle as-counter)
  (cond
    ((zero? as-counter) middle)
    ((> as-counter 1) (+ as-counter middle))
    ((<= (+ 11 middle) 21) (+ 11 middle))
    (else (+ 1 middle))
  )
)

(define (char-appearances List character)
  (cond
    ((empty? List) 0)
    ((equal? character (car List)) (+ 1 (char-appearances (cdr List) character)))
    (else (char-appearances (cdr List) character))
  )
)

(define (everyone-done? players)
  (cond
    ((empty? players) #t)
    ((get-player-state players) #f)
    (else (everyone-done? (cdr players)))
  )
)
 


(define (hand-out-circle board-list n)
  (cond
    ((zero? n) board-list)
    (else
     (hand-out-circle (pass-turn (hand-out board-list 1)) (- n 1))
    )
  )
)



(define (quicksort List)
    (cond
        ((null? List) '())
        (else   
            (append
             (quicksort (filtro (lambda (x) (<= x (car List))) (cdr List)))
             (list (car List))
             (quicksort (filtro (lambda (x) (> x (car List))) (cdr List))))
        )
    )
)

(define (filtro condicion List)
    (cond
        ((null? List) '())
        ((condicion (car List)) (cons (car List) (filtro condicion (cdr List))))
        (else (filtro condicion (cdr List)))
    )
)

;(define a (BCEj1 '(Brian Brian2 Brian3)))
;(hand-out a 2)
;(hand-out-circle a 8)

