#lang racket/gui
#|
Autores: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Luis Andrey Zuñiga Hernandez
Instituto Tecnologico de Costa Rica
Area academica de ingenieria en computadores
User Interface
19 de marzo del 2021
|#

;Librarys used
(require "Logica.rkt")
(require 2htdp/image
         (only-in mrlib/image-core render-image))
(provide (all-defined-out))

;Game-state Variables
(define board-list '()) ; ((cards-available) ((player1 (cards) state) ... ))
(define player-list '())
(define positions '((350 100) (130 290) (350 320) (580 290)))
(define score-table '())
(define font-size 30)

;Grafical interface
(define image1 (make-object bitmap% "Blackjack.jpg"))
(define my-frame  (new frame% [label "Black CE Jack"]
       [width 300]
       [height 400]
       [style '(no-resize-border)]))
(define mcan%
  (class canvas%
    (override  on-paint)
    (define on-paint
      (lambda()(send (send this get-dc) 
      draw-bitmap image1 0 0)))        
    (super-instantiate())))

(define mcan (new mcan% (parent my-frame)
                  (min-width (image-width image1))
                  (min-height (image-height image1))))

(define drawer (send mcan get-dc))

(define my-frame2  (new frame% [label "Black CE Jack Score Table"]
       [width 400]
       [height 400]
       [style '(no-resize-border)]))

(define mcan2 (new mcan% (parent my-frame2)
                  (min-width 700)
                  (min-height 400)) )

(define row1
  (new horizontal-panel%
       [parent my-frame]
       [min-width   30]
       [min-height   50]
       [style       '(border)]
       [stretchable-height #f]
       [horiz-margin 220]))

(define ask-card (new button%
                    [parent row1]
                    [label "Pedir carta"]
                    [horiz-margin 20]

                    [callback (lambda (button event)
                                (cond
                                  ((= 0 (length (get-deck board-list))) (writeln "no hay cartas"))
                                  (else   
                                   (set! board-list (hand-out board-list 1))
                                   (draw-everyone-cards (get-players board-list) #f)
                                   (draw-image "Deck/52.png" 700 11 0.38001)
                                   (draw-text (number->string (length (get-deck board-list))) 40 "black" 720 40)
                                   )
                                )
                           )]
                    ))

(define stay (new button%
                    [parent row1]
                    [label "Plantarse"]
                    [horiz-margin 20]
                    [callback (lambda (button event)
                                (set! board-list (set-stay board-list))
                                (cond
                                  ((everyone-done? (get-players board-list))
                                   (set! board-list (crupier-IA (set-crupier-first board-list)))
                                   
                                   (draw-everyone-cards (get-players board-list) #t)
                                   (set! score-table (sort-scores(get-scores (get-players board-list))))
                                   
                                   (set! drawer (send mcan2 get-dc))
                                   ;(send drawer set-font (make-object font% font-size 'default))
                                   (show-scores)
                                    
                                  )
                                  (else
                                   (draw-label (current-player-name (get-players board-list)) 20 50 90 50)
                                  )
                    ))]))

(define pass (new button%
                    [parent row1]
                    [label "Terminar turno"]
                    [horiz-margin 20]
                    [callback (lambda (button event)
                                (set! board-list (pass-turn board-list))
                                (draw-label (current-player-name (get-players board-list)) 20 50 90 50)
                                )]))

#|
description: a function that loads an image
inputs:
  path: string? //a string that is the file to open
  factor: number? // a number that multiplies the size of the image
output: #<bitmap> //the bitmap of the image
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (load-bitmap path factor)
  (scale factor (bitmap/file path))
)

#|
description: a function that draws an image to the window
inputs:
  path: string? //a string that is the file to open
  factor: number? // a number that multiplies the size of the image
  x: number? // position x in the window to place the picture
  y: number? // position y in the window to place the picture
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (draw-image path x y factor)
  (render-image (load-bitmap path factor) drawer x y)
)

#|
description: a function that draws text to the screen
inputs:
  text: string? //a string to write in the screen
  font-size: number? // a number to estlabish the size of the text
  color: string? //a string that names the color
  x: number? // position x in the window to place the picture
  y: number? // position y in the window to place the picture
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (draw-text text font-size color x y)
  (render-image (text/font
   text
   font-size
   color
   #f
   "roman"
   "italic"
   "bold"
   #f
  )
  drawer x y)
)
#|
description: a function that checks if the list only contains quotes
inputs:
  players: List? //A list containing quotes 
output: #t if it meets the criteria defined, #f otherwise
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (check-quotes players)
  (cond
    ((empty? players) #t)
    ((not(symbol? (car players))) #f)
    (else (check-quotes (cdr players)))
  )
)

#|
description: a function that validates the list
inputs:
  players: List? //A list containing 1 to 3 quotes 
output: #t if it meets the criteria defined, #f otherwise
Autor: Brian Wagemans Alvarado
|#
(define (validate-list players)
  (cond
    ([not (list? players)] #f)
    ((> (length players) 3) #f)
    ((empty? players) #f)
    ((not [check-quotes players]) #f)
    (else #t)
  )
)

#|
description: a function that starts the game using the symbols from the list as names.
inputs:
  players: List? //A list containing 1 to 3 quotes 
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (BCEj players)
  (cond
   ((validate-list players); checks that the player list is a valid format for the game. 

        ;(readys the variables to sart the game)
        (set! board-list (start-game players))
        (send my-frame show #t)
        (set! player-list (append '(crupier) players))
        (set! positions (get-n-data positions (+ (length players) 1)))
        (sleep/yield 1)
        (send drawer set-font (make-object font% font-size 'default))
        (draw-everyone-cards (get-players board-list) #f)
        (draw-image "Deck/52.png" 687 11 0.38001)
        (draw-image "Deck/52.png" 690 11 0.38001)
        (draw-image "Deck/52.png" 693 11 0.38001)
        (draw-image "Deck/52.png" 696 11 0.38001)
        (draw-image "Deck/52.png" 700 11 0.38001)
        (draw-text (number->string (length (get-deck board-list))) 40 "black" 720 40)
        (draw-players-names players (cdr positions))
        (draw-label (car players) 20 50 90 50)
        
   )(else "lista de nombres no es valida") ;if its is not valid returns a message to the user
  )
)

#|
description: a function that draws the player names in the window
inputs:
  players: list? //a list that contains the names of the players
  positions: posn? // a tuple that defines the position of the name
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (draw-players-names players positions)
  (cond
    ((empty? players) '())
    (else
     (draw-text (symbol->string (car players)) 30 "white" (caar positions) (+ 120 (cadar positions)))
     (draw-players-names (cdr players) (cdr positions))
    )
  )
)
#|
description: a function that draws the label in the window
inputs:
  player-name: string? // string that identifies the player, and is the text to draw
  x: number? // position x in the window to place the picture
  y: number? // position y in the window to place the picture
  width: number? // identifies the width of the text
  height: number? // identifies the number of the text
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (draw-label player-name x y width height)
  (render-image (rectangle width height "solid" "dark green") drawer x y)
  (draw-text (symbol->string player-name) 20 "black" x y)
)
#|
description: a function that draws the cards in the window
inputs:
  card-list: list? // a list with the card values of the players
  x: number? // position x in the window to place the picture
  y: number? // position y in the window to place the picture
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (draw-cards card-list X Y)
  (cond
    ((empty? card-list) '())
    (else (draw-image (create-path (car card-list)) X Y 0.38)
          (draw-cards (cdr card-list) (+ X 15) Y)
     )
   )
)

#|
description: a function that draws all the cards out of the deck
inputs:
  players: list? // a list with all the players and the corresponding cards
  finished: boolean? // boolean that represents if the game is finished
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (draw-everyone-cards players finished)
(cond
    ((empty? players) '())
    (else (draw-all-cards (current-player-name players) (current-player-cards players) finished)
          (draw-everyone-cards (cdr players) finished)
     )
  )
)
#|
description: a function that draws the cards of the player
inputs:
  player-name: string? // string that identifies the player
  player-cards: list? // list that contains the cards of the player
  finished: boolean? // boolean that represents if the game is finished
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (draw-all-cards player-name player-cards finished)
  (cond
    ((and (equal? player-name 'crupier) (not finished))
     (draw-cards
      (append '(52) (cdr player-cards))
      (car (get-coords 'crupier player-list positions))
      (cadr (get-coords 'crupier player-list positions)))
    )
    (else
     (draw-cards player-cards
                (car (get-coords player-name player-list positions))
                (cadr (get-coords player-name player-list positions)))
    )
  )
)
#|
description: a function that gets the coordanates asociated to a player
inputs:
  player-names: list? // list with the players names
  coords-list: list? // list that contains the coordanates of the player
  player: string? // string that identifies the player
output: #<void>
Autor: Alejandro Vasquez Oviedo, Brian Wagemans Alvarado, Andrey Zuñiga Hernandez
|#
(define (get-coords player players-names coords-list)
  (cond
    ((empty? players-names) '())
    ((equal? player (car players-names)) (car coords-list))
    (else (get-coords player (cdr players-names) (cdr coords-list)))
  )
)

#|
description: a function that returns the path of the image
inputs:
  n: number? // number that represents the card
output: string //path of the image
Autor: Andrey Zuñiga Hernandez
|#
(define (create-path n)
  (string-append "Deck/" (number->string n) ".png"))
(define (draw-table table x y)
  (cond
    ((empty? table) '())
    (else
       (draw-text (symbol->string (caar table)) font-size "white" x y)
       (draw-text (number->string (cadar table)) font-size "white" (+ x 150) y)
       (draw-table (cdr table) x (+ font-size y))
     )
   )
)
#|
description: a function that shows the second windows, with the scores
inputs:
output: #<void>
Autor: Alejandro Vasquez Oviedo
|#
(define (show-scores)
  (send my-frame2 show #t)
  (sleep/yield 1)
  (draw-image "scoretable.png" 0 0 1)
  (draw-table score-table 250 90)
)
