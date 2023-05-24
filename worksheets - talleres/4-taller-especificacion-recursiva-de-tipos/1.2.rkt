#lang eopl
#|
Abstracción de datos

Este ejercicio se resolvió, utilizando la implementación del TAD, a diferencia
De otros ejercicios anteriores (pej. de arboles) que fueron solución directamente
en listas (implementación de racket)
|#

;1. Dada la siguiente gram ́atica de un  ́arbol binario:
;<arbol-b> ::= (arbol-vacio) ’()
;          ::= (hoja-s) <symbol>
;          ::= (hoja-n) <int>
;          ::= (nodo) <symbol> <arbol-b> <arbol-b>
;La cual representa una lista de lista de s ́ımbolos, construya las funciones:
;a) in-S? (determinar si un elemento pertenece a esta gram ́atica)
;b) buscar-simbolo: Retorna verdadero si el s ́ımbolo est ́a presente en la lista
;c) comparar-arboles: recibe dos  ́arboles, retorna verdadero si ambas estructuras son
;exactamente las mismas, la comparaci ́on tiene que ser recursiva

;función auxiliar
(define contar-elementos
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (contar-elementos (cdr lst))))))

;constructores
(define arbol-vacio
  (lambda ()
    '()))
(define hoja-s
  (lambda (symbol)
    (if (symbol? symbol)
        (list 'hoja-s symbol)
        (eopl:error "Contract violation, constructor hoja-s receive a symbol as argument"))))
(define hoja-n
  (lambda (number)
    (if (integer? number)
        (list 'hoja-n number)
        (eopl:error "Contract violation, constructor hoja-n receive a number as argument"))))
(define nodo
  (lambda (key-symbol h1 h2)
    (if (and (symbol? key-symbol)
             (or
     (arbol-vacio? h1)
     (hoja-n? h1)
     (hoja-s? h1)
     (nodo? h1)
     (arbol-vacio? h2)
     (hoja-n? h2)
     (hoja-s? h2)
     (nodo? h2))
     )
        (list 'nodo key-symbol h1 h2)
        (eopl:error "Contract violation"))))
;predicados
(define arbol-vacio?
  (lambda (exp)
    (null? exp)))
(define hoja-s?
  (lambda (exp)
    (and
     (list? exp)
     (not (null? exp))
     (eqv? (car exp) 'hoja-s)
     (symbol? (cadr exp))
     (null? (cddr exp)))))

(define hoja-n?
  (lambda (exp)
    (and
     (list? exp)
     (not (null? exp))
     (eqv? (car exp) 'hoja-n)
     (integer? (cadr exp))
     (null? (cddr exp)))))
(define nodo?
  (lambda (exp)
    (and
     (list? exp)
     (equal? (contar-elementos exp) 4)
     (symbol? (car exp))
     (eqv? (car exp) 'nodo)
     (symbol? (cadr exp))
     (or
      (arbol-vacio? (caddr exp))
      (hoja-n? (caddr exp))
      (hoja-s? (caddr exp))
      (nodo? (caddr exp))
      (arbol-vacio? (cadddr exp))
      (hoja-n? (cadddr exp))
      (hoja-s? (cadddr exp))
      (nodo? (cadddr exp))))))
;pruebas
(display (hoja-s? (hoja-s 'r)))
(newline)
(display (hoja-n? (hoja-n 1)))
(newline)
(display (nodo? (nodo 'r (hoja-n 4) (nodo 'g (hoja-n 4) (hoja-s 'u)))))
(newline)
;extractores
(define hoja-s->valor
  (lambda (exp)
    (if (hoja-s? exp)
        (cadr exp)
        (eopl:error "contract violation")
        )))
(define hoja-n->valor
  (lambda (exp)
    (if (hoja-n? exp)
        (cadr exp)
        (eopl:error "contract violation")
        )))
(define nodo->key
  (lambda (exp)
    (if (nodo? exp)
        (cadr exp)
        (eopl:error "contract violation"))))
(define nodo->h1
  (lambda (exp)
    (if (nodo? exp)
        (caddr exp)
        (eopl:error "contract violation"))))
(define nodo->h2
  (lambda (exp)
    (if (nodo? exp)
        (cadddr exp)
        (eopl:error "contract violation"))))

;area del programador
;a) in-S? (determinar si un elemento pertenece a esta gram ́atica)
(define in-S?
  (lambda (exp)
    (or
     (arbol-vacio? exp)
     (hoja-n? exp)
     (hoja-s? exp)
     (nodo? exp))))
;b) buscar-simbolo: Retorna verdadero si el s ́ımbolo est ́a presente en la lista
(define buscar-simbolo
  (lambda (arb sym)
    (cond
      ;verificar que los argumentos esten bien ingresados
      [(or (not (in-S? arb)) (not (symbol? sym))) (eopl:error "contract violation")]
      ;si todo esta bien comienza a buscar
      [(arbol-vacio? arb) #f]
      [(hoja-s? arb) (equal? (hoja-s->valor arb) sym)]
      [(nodo? arb) (or (equal? (cadr arb) sym) (buscar-simbolo (caddr arb) sym) (buscar-simbolo (cadddr arb) sym))]
      [else #f])))
;pruebas
(define arb1
  (nodo 'k
        (nodo 't
              (nodo 'i
                    (nodo 'r (hoja-n 4) (hoja-s 'o))
                    (arbol-vacio))
              (hoja-n 3))
        (nodo 'z (arbol-vacio) (arbol-vacio))))

(display (buscar-simbolo arb1 'j))

;c) comparar arboles
(define comparar-arboles
  (lambda (arb1 arb2)
    (cond
      [(and (null? arb1) (null? arb2)) #t]
      [(and (hoja-s? arb1) (hoja-s? arb2)) (equal? (hoja-s->valor arb1) (hoja-s->valor arb2))]
      [(and (hoja-n? arb1) (hoja-n? arb2)) (equal? (hoja-n->valor arb1) (hoja-n->valor arb2))]
      [(and (nodo? arb1) (nodo? arb2) (equal? (nodo->key arb1) (nodo->key arb2)))
       (and (comparar-arboles (nodo->h1 arb1) (nodo->h1 arb2))
            (comparar-arboles (nodo->h2 arb2) (nodo->h2 arb2)))]
      [else #f])))

(newline)
(display (comparar-arboles arb1 arb1))
(newline)
(display (comparar-arboles
          ;arb1
          (nodo 'k
        (nodo 't
              (nodo 'i
                    (nodo 'r (hoja-n 43) (hoja-s 'o))
                    (arbol-vacio))
              (hoja-n 3))
        (nodo 'z (arbol-vacio) (arbol-vacio)))
          ;arb2
          (nodo 'k
        (nodo 't
              (nodo 'i
                    (nodo 'r (hoja-n 4) (hoja-s 'o))
                    (arbol-vacio))
              (hoja-n 3))
        (nodo 'z (arbol-vacio) (arbol-vacio)))))












       





      