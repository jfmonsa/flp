#lang eopl
;1. Dada la siguiente gram ́atica de un  ́arbol binario:

;  <arbol-b> ::= (arbol-vacio) ’()
;    ::= (hoja-s) <symbol>
;    ::= (hoja-n) <int>
;    ::= (nodo) <symbol> <arbol-b> <arbol-b>

;La cual representa una lista de lista de s ́ımbolos, construya las funciones:
;  a) in-S? (determinar si un elemento pertenece a esta gram ́atica)
;  b) buscar-simbolo: Retorna verdadero si el s ́ımbolo est ́a presente en la lista
;  c) comparar-arboles: recibe dos  ́arboles, retorna verdadero si ambas estructuras son
;    exactamente las mismas, la comparaci ́on tiene que ser recursiva.

;a)
(define in-S?
  (lambda (arb)
    (cond
      [(null? arb) #t] ;caso lista vacia
      [(and (number? (car arb)) (null? (caar arb))) #t] ;caso hoja-n
      [(and (symbol? (car arb)) (null? (caar arb))) #t] ;caso hoja-s
      [(and (symbol? (car arb)) (in-S? (cadr arb)) (in-S? (caddr arb))) #t] ;caso nodo - llamado recursivo
      [else #f] ;Si no cumple con ninguna regla de producción, entonces no pertenece a la gramática
      )))

(define arb1
  '('a ('b) ('c 1))
  )
(display (in-S? arb1)) ;f
(newline)
(display (in-S? '())) ;t
(newline)

;b)
(define (buscar arbol simbolo)
  (cond ((null? arbol) #f) ; Si el árbol está vacío, devuelve false
        ((eq? simbolo (car arbol)) #t) ; Si el símbolo es la raíz del árbol, devuelve true
        ((buscar (cadr arbol) simbolo)) ; Busca en el hijo izquierdo
        ((buscar (caddr arbol) simbolo)) ; Busca en el hijo derecho
        (else #f))) ; Si el símbolo no está en el árbol, devuelve false


(define buscar-symbol
  (lambda (arb s)
    (cond
      [(null? arb) #f]
      [(and (symbol? (car arb)) (= s (car arb))) #t]
      [else (or
             (buscar-symbol (cadr arb) s)
             (buscar-symbol (caddr arb) s)
                            )])))

;(display (buscar-symbol '('c) 'c))
(newline)

;c
(define (arboles-iguales? arbol1 arbol2)
  (cond
    ; Si ambos árboles están vacíos, son iguales
    ((and (null? arbol1) (null? arbol2)) #t)
    ; Si los árboles tienen distinto número de nodos, no son iguales
    ((or (null? arbol1) (null? arbol2)) #f)
    ; Si las raíces de los árboles son diferentes, no son iguales
    ((not (equal? (car arbol1) (car arbol2))) #f)
    ; Compara los subárboles izquierdos y derechos recursivamente
    (else (and (arboles-iguales? (cadr arbol1) (cadr arbol2))
               (arboles-iguales? (caddr arbol1) (caddr arbol2))))))

(define arbol1 '((nodo a (hoja-s b) (hoja-n 42))
                 (nodo c (hoja-s d) (hoja-s e))))

(define arbol2 '((nodo a (hoja-s b) (hoja-n 42))
                 (nodo c (hoja-s d) (hoja-s e))))

(define arbol3 '((nodo a (hoja-s b) (hoja-n 42))
                 (nodo c (hoja-s d) (hoja-s f))))

(arboles-iguales? arbol1 arbol2) ; devuelve #t
(arboles-iguales? arbol1 arbol3) ; devuelve #f