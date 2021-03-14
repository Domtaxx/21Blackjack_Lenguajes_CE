#lang racket/gui

(require "Logica.rkt")
(require 2htdp/image
         (only-in mrlib/image-core render-image))
(provide (all-defined-out))
(define board-list '())
(define player-list '())
(define positions '((350 100) (130 290) (350 320) (580 290)))
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




(define row1
  (new horizontal-panel%
       [parent my-frame]
       [min-width   30]
       [min-height   50]
       [style       '(border)]
       [stretchable-height #f]
       [horiz-margin 150]))

(define ask-card (new button%
                    [parent row1]
                    [label "Pedir carta"]
                    [horiz-margin 20]

                    [callback (lambda (button event)
                                (set! board-list (hand-out board-list 1))
                                (draw-everyone-cards (get-players board-list) #f)

                           )]
                    ))

(define stay (new button%
                    [parent row1]
                    [label "Plantarse"]
                    [horiz-margin 20]
                    [callback (lambda (button event)
                                (cond
                                  ((everyone-done? (get-players board-list))
                                   (set! board-list (crupier-IA (set-crupier-first board-list)))
                                   
                                   (draw-everyone-cards (get-players board-list) #t)
                                   (writeln (sort-scores(get-scores (get-players board-list))))
                                  )
                                  (else
                                   (set! board-list (set-stay board-list))
                                  ; (writeln board-list)
                                  )
                    ))]))

(define pass (new button%
                    [parent row1]
                    [label "Terminar turno"]
                    [horiz-margin 20]
                    [callback (lambda (button event)
                                (set! board-list (pass-turn board-list))
                                (writeln board-list)
                                )]))

(define show-cards (new button%
                    [parent row1]
                    [label "Repartir"]
                    [horiz-margin 20]
                    [callback (lambda (button event)
                                (draw-everyone-cards (get-players board-list) #f)
                                (draw-image "Deck/52.png" 687 11 0.38001)
                                (draw-image "Deck/52.png" 690 11 0.38001)
                                (draw-image "Deck/52.png" 693 11 0.38001)
                                (draw-image "Deck/52.png" 696 11 0.38001)
                                (draw-image "Deck/52.png" 700 11 0.38001)
                    )]
                    ))
(define drawer (send mcan get-dc))


(define (load-bitmap path factor)
  (scale factor (bitmap/file path))
)

(define (draw-image path x y factor)
  (render-image (load-bitmap path factor) drawer x y)
)


(define (BCEj players)
  (set! board-list (start-game players))
  (send my-frame show #t)
  (set! player-list (append '(crupier) players))
  (set! positions (get-n-data positions (+ (length players) 1)))
  (write positions)
  (draw-everyone-cards (get-players board-list) #f)
  
)

(define (draw-cards card-list X Y)
  (cond
    ((empty? card-list) '())
    (else (draw-image (create-path (car card-list)) X Y 0.38)
          (draw-cards (cdr card-list) (+ X 15) Y)
     )

   )

)
(define (draw-everyone-cards players finished)
  (cond
    ((empty? players) '())
    (else (draw-all-cards (current-player-name players) (current-player-cards players) finished)
          (draw-everyone-cards (cdr players) finished)
     )


  )


)
(define (draw-all-cards player-name player-cards finished)
  (cond
    ((and (equal? player-name 'crupier) (not finished))
     (draw-cards
      (append '(52) (cdr player-cards))
      (car (get-coords player-name player-list positions))
      (cadr (get-coords player-name player-list positions)))
    )
    (else
     (draw-cards player-cards
                (car (get-coords player-name player-list positions))
                (cadr (get-coords player-name player-list positions)))
    )
  )
)
(define (get-coords player players-names coords-list)
  (cond
    ((equal? player (car players-names)) (car coords-list))
    (else (get-coords player (cdr players-names) (cdr coords-list)))

  )
)

(define (create-path n)
  (string-append "Deck/" (number->string n) ".png"))

