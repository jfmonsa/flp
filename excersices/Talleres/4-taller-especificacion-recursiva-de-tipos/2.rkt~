#lang eopl
;2. Dada la siguiente gram ́atica de una lista de pares
;  <pair-lst> ::=(vacia) ’()
;  ::=(novacia) <pair> <pair-lst>
;  <pair> ::=(par) <int> <int>

;Realice las funciones

;  a) in-S? (determinar si un elemento pertenece a esta gram ́atica)
;  b) buscar-numero: Retorna verdadero si el n ́umero est ́a presente en la lista de pares
;  c) sumar-pares, esta funci ́on recibe una lista de pares y retorna un par que representa

;la suma de cada uno de los elementos. Ejemplo ((1 2) (2 3)(3 4)) retorna (6 9), ’()
;retorna (0 0)

;verifica que sea una lista de pares
;(define in-S?
;  (lambda (pir)
;    (cond
;      [else (null? pir) #t]
;      [(and (number? (car pir)) (number? (cadr pir))) (in-S? (caddr pir))]
;;      [else #f])))

;verifica que sea un par
(define in-s?-aux
  (lambda (pir)
    (cond
      [(null? pir) #t]
      [else (and (number? (car pir)) (number? (cadr pir)))])))

(display (in-s?-aux '(1 2)))