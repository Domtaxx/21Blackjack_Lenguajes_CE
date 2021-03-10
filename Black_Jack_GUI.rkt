#lang racket/gui

(require 2htdp/image)
(provide (all-defined-out))


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
       [horiz-margin 220]))


(define ask-card (new button%
                    [parent row1]
                    [label "Pedir carta"]
                    [horiz-margin 20]))

(define as_values
  (new radio-box%
       [label "Valor del as"]
       [choices '("1" "11")]
       [parent row1]
       [style '(horizontal)]))

(define drawer (send mcan get-dc))

(define stay (new button%
                    [parent row1]
                    [label "Plantarse"]
                    [horiz-margin 20]
                    [callback (lambda (button event) (send drawer erase))]))
(send my-frame show #t)