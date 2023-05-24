#lang eopl
;1. Dada la siguiente gram ́atica de un  ́arbol binario:

;<arbol-b> ::= (arbol-vacio) ’()
;::= (hoja-s) <symbol>
;::= (hoja-n) <int>
;::= (nodo) <symbol> <arbol-b> <arbol-b>

;La cual representa una lista de lista de s ́ımbolos, construya las funciones:
; a) in-S? (determinar si un elemento pertenece a esta gram ́atica)
;predicados
(define arbol-vacio? null?)
(define hoja-s?
  (lambda (exp)
    (and (not (null? exp)) (list? exp) (symbol? (car exp)) (null? (cdr exp)))
    ))
(define hoja-n?
  (lambda (exp)
    (and (not (null? exp)) (list? exp) (integer? (car exp)) (null? (cdr exp)))
    ))
(define nodo?
  (lambda (exp)
    (and
     (list? exp)
     (not (or (null? (cdr exp)) (null? (cddr exp))))
     (null? (cdddr exp))
     (symbol? (car exp))
     (or
      (arbol-vacio? (cadr exp))
      (hoja-s? (cadr exp))
      (hoja-n? (cadr exp))
      (nodo? (cadr exp))
      )
     (or
      (arbol-vacio? (caddr exp))
      (hoja-s? (caddr exp))
      (hoja-n? (caddr exp))
      (nodo? (caddr exp)))
     )))
(define in-S?
  (lambda (exp)
    (or
     (arbol-vacio? exp)
     (hoja-s? exp)
     (hoja-n? exp)
     (nodo? exp)
     )))

;pruebas
;(display (hoja-s? '(y)))
;(newline)
;(display (hoja-n? '(5)))
;(newline)
;(display (nodo? '(y (5) (4))))
;(newline)
(define arb1 '(t (t (z (3) ()) ()) (t () ())))
(display (in-S? arb1))
(newline)

; b) buscar-simbolo: Retorna verdadero si el s ́ımbolo est ́a presente en la lista
;extractores
(define buscar-simbolo
  (lambda (arb symbol)
    (cond
      [(null? arb) #f] ;caso base
      [(hoja-s? arb) (equal? (car arb) symbol)]
      [(nodo? arb) (or (equal? (car arb) symbol) (buscar-simbolo (cadr arb) symbol) (buscar-simbolo (caddr arb) symbol))]
      [else #f])))
;prueba
(display (buscar-simbolo '(z (5) (b (t (z (g (0) (10)) (y)) (r)) ())) 'g))
(newline)

; c) comparar-arboles: recibe dos  ́arboles, retorna verdadero si ambas estructuras son
;exactamente las mismas, la comparaci ́on tiene que ser recursiva.
(define comparar-arboles
  (lambda (arb1 arb2)
  (cond
    [(and (null? arb1) (null? arb2)) #t] ;los dos árboles son nulos
    [(not (equal? (car arb1) (car arb2))) #f] ; las raíces de los dos árboles son diferentes
    [(and (nodo? arb1) (nodo? arb2) (equal? (car arb1) (car arb2))) ; los dos árboles tienen la misma raíz
     (and (comparar-arboles (cadr arb1) (cadr arb2)) ; comparamos los subárboles izquierdos
          (comparar-arboles (caddr arb1) (caddr arb2))) ; comparamos los subárboles derechos
     ]
    [else #f]

    )))


(display (comparar-arboles
 '(r () (y (f (4) (3)) (t)))
 '(r () (y (f (4) (2)) (t)))
 ))
;'(r () (g (v (6) (6)) (b (4) (5))))



