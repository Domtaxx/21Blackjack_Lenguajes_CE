#lang racket 
(require "Black_Jack_GUI.rkt")
;lista de jugadores((cards-available) ((jugado1 (cartas) state) (jugador2 (cartas) state) ...) )
(define (BCEj List)
  (append (list (shuffle(give-deck 51))) (list (BCEj-aux List  '())))
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

(define (current-player-name players)
  (caar players)
)
(define (current-player-cards players)
  (cadar players)
)
(define (other-players players)
  (cdr players)
)
(define (get-player-state player)
  (caddar player)
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

(define (hand-out board-list card-num)
  (cond
    ((<= card-num 0) board-list)
    (else
     (hand-out (manage-board-list board-list) (- card-num 1))
    )
  )
)

;
(define (pass-turn board-list)
  (pass-turn-aux (get-deck board-list) (get-players board-list))
)

(define (pass-turn-aux deck player-list)
  (append
   (list deck)
   (list {append (other-players player-list)
           (list(list
                 (current-player-name player-list)
                 (current-player-cards player-list)
                 (get-player-state player-list)
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




(define (start-game list-players turno)
  
  (cond
    ((game-over list-players )
     {
      1 ;FINALIZAR JUEGO
     }
    )
   (else
     1 ;AGREGAR CARTAS list-players
   )
  )
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
(define a (BCEj '(Brian Brian2 Brian3)))
(hand-out a 2)
;(pass-turn)

