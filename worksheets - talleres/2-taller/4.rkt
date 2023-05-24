;Dise ̃na la funci ́on predicado-conteo esta recibe una funci ́on predicado f , y una lista l.
;Se retorna el n ́umero de elementos en la lista l que cumnplen con el predicado f , ejemplo:

#lang eopl
(define predicado-conteo
  (lambda (func-pred lst)
    (cond
      [(null? lst) 0]
      [(func-pred (car lst)) (+ 1 (predicado-conteo func-pred (cdr lst)))]
      [else (predicado-conteo func-pred (cdr lst))]
     )))

(display (predicado-conteo number? '(0 0 1 1 2 "hola")))
(newline)
(display (predicado-conteo number? '("perreo"  0 0 1 2 "hola")))
