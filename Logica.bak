#lang racket 
(require "Black_Jack_GUI.rkt")
(define (BCEj List)
  (BCEj-aux List '((Crupier ())))
  (start-game)
)
(define (BCEj-aux List newList)
  (cond
    ((empty? List) newList)
    (else (BCEj-aux (cdr List) (append newList (list (list (car List) '() )))))
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
  (append (other-players players-list) (list (list (current-player-name players-list) (addCard (current-player-cards players-list) cards-num ) )))
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







(addCard '(Brian ()) 2)
;(BCEj '(Brian Brian2 Brian3))
