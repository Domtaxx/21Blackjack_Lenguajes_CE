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

(BCEj '(Brian Brian2 Brian3))