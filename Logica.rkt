#lang racket
#|
Autores: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Luis Andrey Zuñiga Hernandez
Instituto Tecnologico de Costa Rica
Area academica de ingenieria en computadores
Game logic
19 de marzo del 2021
|#

;Librarys used
(require racket/trace)
(provide (all-defined-out))

#|
description: a function that returns the first board-deck list with cards handed out
inputs:
  players: list? // list that contains the player names
output: list // first state of board-list list
Autor: Andrey Zuñiga Hernandez
|#
(define (start-game players)
  (set-stay (hand-out-circle (BCEj1 players) (* 2 (+ 1 (length players)))))
)
#|
description: a function that returns the first board-deck list without handing-out cards
inputs:
  List: list? // list that contains the player names
output: list // first state of board-list list
Autor: Andrey Zuñiga Hernandez
|#
(define (BCEj1 List)
  (append (list (shuffle(give-deck 51))) (list (BCEj-aux (append '(crupier) List ) '())))
)
#|
description: a function that returns the first board-deck list
inputs:
  List: list? // list that contains the player names
output: list // gives back the first board-list list
Autor: Andrey Zuñiga Hernandez
|#
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
#|
description: a function that changes the player-list in board-list
inputs:
  board-list: list? // board-list
  new-players: list of new players // list containing the new players of board-list
output: list// update of the player list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (change-player-list board-list new-players)
  (append (list (get-deck board-list)) (list new-players))
)
#|
description: a function that facilitates the call of the current player
inputs:
  players: list? // player-list of the board-list
output: list// containing the cards and the name of the player
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (current-player players)
  (car players)
)
#|
description: a function that facilitates the call of the current player
inputs:
  players: list? // player-list of the board-list
output: symbol// name of the current player
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (current-player-name players)
  (caar players)
)
#|
description: a function that facilitates the call of the current player's cards
inputs:
  players: list? // player-list of the board-list
output: list// cards of the current player
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (current-player-cards players)
  (cadar players)
)
#|
description: a function that facilitates the call of the player list witout the current player
inputs:
  players: list? // player-list of the board-list
output: list// other players in the list of players
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (other-players players)
  (cdr players)
)
#|
description: a function that facilitates the call of the current player's state
inputs:
  players: list? // player-list of the board-list
output: boolean// state of the current player
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (get-player-state players)
  (caddar players)
)
#|
description: a function that facilitates the call of the card deck
inputs:
  board-list: list? // board-list
output: list// cards on the deck
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (get-deck board-list)
  (car board-list)
)
#|
description: a function that facilitates the call of the first card in the deck
inputs:
  board-list: list? // board-list
output: number// first card of the deck
Autor: Alejandro Vasquez Oviedo
|#
(define(get-first-card board-list)
  (car (get-deck board-list))
)
#|
description: a function that facilitates the call of the players list
inputs:
  board-list: list? // board-list
output: list// list containing the players,their cards and state
Autor: Andrey Zuñiga Hernandez
|#
(define (get-players board-list)
  (cadr board-list)
)
#|
description: a function that facilitates the call of the current player's state
inputs:
  board-list: list? // board-list
output: boolean// state of the current player
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (get-current-player-state board-list)
  (get-player-state (get-players board-list))
)
#|
description: a function that facilitates the call of the current player
inputs:
  board-list: list? // board-list
output: list// list containg the current player, his cards and state 
Autor: Brian Wagemans Alvarado
|#
(define (get-first-player board-list)
  (current-player (get-players board-list))
)
#|
description: a function that facilitates the call of the players list, without the current player
inputs:
  board-list: list? // board-list
output: list// list of players without the current player
Autor: Andrey Zuñiga Hernandez
|#
(define (get-other-players board-list)
  (other-players (get-players board-list))
)
#|
description: a function that changes the player list, adding a card to the current player
inputs:
  card: card to add
  player-list: list? // player-list
output: list// update of the player list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
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
#|
description: a function that changes the current player
inputs:
  deck: deck of the board-list
  player-list: list? // player-list
  state: boolean // new player state
output: list// update of the player list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (manage-board-list board-list)
  (append (list(cdr(get-deck board-list)))
          (list (add-card (get-first-card board-list) (get-players board-list)))
          )
)
#|
description: a function that hands out a card to the current player
inputs:
  board-list: list? // board-list
  card-num: number? //amount cards to add
output: list// update of the board-list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (hand-out board-list card-num)
  (cond
    ((<= card-num 0) board-list)
    (else
     (hand-out (manage-board-list board-list) (- card-num 1))
    )
  )
)
#|
description: a function that hands out a card to all the players
inputs:
  board-list: list? // board-list
  n: number? //amount cards to add
output: list// update of the board-list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (hand-out-circle board-list n)
  (cond
    ((zero? n) board-list)
    (else
     (hand-out-circle (pass-turn (hand-out board-list 1)) (- n 1))
    )
  )
)
#|
description: a function that changes the current player
inputs:
  board-list: list? // board-list
output: list// update of the board-list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (pass-turn board-list)
  (cond
    ((everyone-done? (get-players board-list)) board-list)
    (else
     (change-player-list board-list
                      (pass-turn-aux (get-players board-list) #f))
    )
  )
)
#|
description: a function that changes the current player
inputs:
  players: list? // player-list
  done: boolean // true if it has passed the turn at least one time
output: list// update of the player list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
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
#|
description: a function that changes the current player's state and ends the turn
inputs:
  board-list: list? // board-list
output: list// update of the board list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (set-stay board-list)
  (pass-turn (set-state (get-deck board-list) (get-players board-list) #f))
)
#|
description: a function that changes the current player's state
inputs:
  deck: deck of the board-list
  player-list: list? // player-list
  state: boolean // new player state
output: list// update of the board list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (set-state deck player-list state)
  (append
   (list deck)
   (list {append 
           (list(list
                 (current-player-name player-list)
                 (current-player-cards player-list)
                 state
           ))
          (other-players player-list)
   }
  ))
)
#|
description: a function that checks the states of every player
inputs:
  list-players: list? // player-list
output: boolean // true if every player state is false
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
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
#|
description: a function that checks if every player has finished the game
inputs:
  players: list? // player-list
output: boolean // true if every player is done 
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (everyone-done? players)
  (cond
    ((empty? players) #t)
    ((get-player-state players) #f)
    (else (everyone-done? (cdr players)))
  )
)
#|
description: a function that gives a list containing n numbers
inputs:
  n: number? // amount of cards in the deck
output: list // deck list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
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
#|
description: a function that returns the players scores
inputs:
  players: list? // player-list
output: list// returns a list of players, in which the player's name, score,
               difference of the score and 21, amount of cards
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (get-scores players) 
  (cond
    ((empty? players) '())
    (else
     (append
      (list (list
             (current-player-name players)
             (evaluate-as (add-card-values (current-player-cards players))
                          (char-appearances (current-player-cards players) '(0 13 26 39))
             )
             (-
              {evaluate-as
                         (add-card-values (current-player-cards players))
                         (char-appearances (current-player-cards players) '(0 13 26 39))
              }21
             )
             
             (length (current-player-cards players))
            )
      )
      (get-scores (other-players players))
     )
    )
  )
    
)
#|
description: a function that adds the player scores
inputs:
  cards: list? // list of player cards
output: number // score of the player
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (add-card-values cards)
  (cond
    ((empty? cards) 0)
    (else
     (+ (evaluate-card (car cards)) (add-card-values (cdr cards)))
    )
  )
)
#|
description: a function that evaluates a card and assigns a value
inputs:
  deck: list? //deck of the board-list
  player-list: list? // player-list
  state: boolean // new player state
output: list// update of the board list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (evaluate-card card)
  (cond
    ((zero? (remainder card 13)) 0)
    ((>= (remainder card 13) 9) 10)
    (else (+ (remainder card 13) 1))
  )
)

#|
description: a function that evaluates a card and assigns a value
inputs:
  middle: number? // players score without any ace
  as-counter: number? // amount of aces in the player hand
output: number // player score
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (evaluate-as middle as-counter)
  (cond
    ((zero? as-counter) middle)
    ((> as-counter 1) (+ as-counter middle))
    ((<= (+ 11 middle) 21) (+ 11 middle))
    (else (+ 1 middle))
  )
)
#|
description: a function that evaluates a list to see if any of the caracters are present
inputs:
  List: list? // list to check list of characters to check
  character-list: list? // list of characters to check
output: number, amount of apperances
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (char-appearances List character-list)
  (cond
    ((empty? character-list) 0)
    (else (+
           (char-appearances-aux List (car character-list) )
           (char-appearances List (cdr character-list)))
    )

  )
)
#|
description: a function that evaluates a list to see if any of the caracters are present
inputs:
  List: list? // list to check the character
  character: any/c // element to check
output: number, amount of apperances
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (char-appearances-aux List character)
  (cond
    ((empty? List) 0)
    ((equal? character (car List)) (+ 1 (char-appearances-aux (cdr List) character)))
    (else (char-appearances-aux (cdr List) character))
  )
)

#|
description: a function that orders the list
inputs:
  List: list? // list to order
  n: number? // amount of depth
output: list // orderd list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (quicksort List n)
    (cond
        ((null? List) '())
        (else   
            (append
             (quicksort (filtro (lambda (x) (<= x (get-data-by-pos (car List) n))) (cdr List) n) n)
             (list (car List))
             (quicksort (filtro (lambda (x) (> x (get-data-by-pos (car List) n))) (cdr List) n) n))
        )
    )
)
#|
description: a function that orders the list by a certain condition
inputs:
  List: list? // list to order
  n: number? // amount of depth
output: list // ordered list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (filtro condicion List n)
    (cond
        ((null? List) '())
        ((condicion (get-data-by-pos (car List) n))
              (cons (car List) (filtro condicion (cdr List) n))
        )
        (else (filtro condicion (cdr List) n))
    )
)
#|
description: a function that orders the list of scores
inputs:
  List: list? // list to order
output: list // ordered list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (sort-scores List)
   (sort-scores-table (quicksort List 2))
)

#|
description: a function that orders the list of scores
inputs:
  table-values: list? // list to order
output: list // ordered list
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (sort-scores-table table-values)
  (append (quicksort
                     (filtro zero? table-values 2) 3)
                     (reverse (filtro negative? table-values 2))
                     (filtro positive? table-values 2))
)
#|
description: a function that gets n amount of elements from a list
inputs:
  List: list? // list to get data from
  n: number? // amount of elements to get
output: list // first-n amount of elements from List
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (get-n-data List n)
  (cond
    ((zero? n) '())
    (else (cons (car List) (get-n-data (cdr List) (- n 1))))
  )
)

#|
description: a function that returns a certain value from a list, given its position
inputs:
  generic-list: list? // list to serch in
  n: number? // position of the element to get
output: any/c //element in the given position
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (get-data-by-pos generic-list n)
  (cond
    ((>= n (length generic-list)) "se inserto un dato incorrecto")
    ((zero? n) (car generic-list))
    (else
     (get-data-by-pos (cdr generic-list) (- n 1))
    )
  )
)
#|
description: a function that sets the cupier as the first in the player list
inputs:
  player-list: list? // list to order
output: list // ordered list, with cupier first
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (set-crupier-first-aux player-list)
   (cond
    ((equal? 'crupier (current-player-name player-list)) player-list)
    (else (set-crupier-first-aux (append (cdr player-list) (list (car player-list)))))
  )
)
#|
description: a function that orders the list of scores
inputs:
  board-list: list? // list to order
output: list // ordered list with crupier first
Autor: Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (set-crupier-first board-list)
  (change-player-list board-list (set-crupier-first-aux (get-players board-list)))
)

#|
description: a function that orders the list of scores
inputs:
  board-list: list? // board-state list 
output: list // new board list, with the crupiers turn finished
Autor: Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (crupier-IA board-list)
  (cond
    ((<= (evaluate-as (add-card-values (current-player-cards (get-players board-list)))
                      (char-appearances
                          (current-player-cards (get-players board-list))
                          '(0 13 26 39))
                      )
         16)
     (crupier-IA (hand-out board-list 1))
    )
    (else
     board-list
    )
  )
)