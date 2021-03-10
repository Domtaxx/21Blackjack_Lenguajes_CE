#lang racket 
(require "Black_Jack_GUI.rkt")
;lista de jugadores((Crupier (cartas)) (jugado1 (cartas)) (jugador2 (cartas)) ...)
(define (BCEj List)
  (BCEj-aux List '())
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

(define (repartir players-list cards-num)
  (append
   (other-players players-list)
   [list
    [list
     (current-player-name players-list)
     (addCard (current-player-cards players-list) cards-num)
    ]
   ]
  )
)

(define (addCard currentCards cardsNumber)
  (append currentCards (getListNRandom cardsNumber 52))

)

(define (getListNRandom n max)
  (cond
    ((zero? n) '())
    (else (append (list (random max)) (getListNRandom (- n 1) max)))
  )
)

(define (start-game list-players turno)
  
  (cond
    ((game-over list-players)
     {
      1 ;FINALIZAR JUEGO
     }
    )
   (else
     1; AGREGAR CARTAS list-players
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
;(addCard '(Brian ()) 2)
(BCEj '(Brian Brian2 Brian3))
