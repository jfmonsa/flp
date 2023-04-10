#lang eopl
;<arbol-t>  ::=  '() (arbol-vacio)
;           ::= <numero> (hoja(num))
;           ::= <simbolo> <arbol-t> <arbol-t> <arbol-t> (nodo(key, h1,h2,h3))

;;Constructores
(define arbol-vacio
  (lambda ()
    (list 'arbol-vacio)))

(define hoja
  (lambda (n)
    (list 'hoja n)))

(define nodo
  (lambda (k h1 h2 h3)
    (list 'nodo k h1 h2 h3)))

(define arbol1
  (nodo
   's
   (nodo 't (hoja 2) (hoja 3) (arbol-vacio))
   (hoja 4)
   (nodo 'k
         (nodo 'p (hoja 4) (hoja 5) (hoja 6))
         (hoja 7)
         (arbol-vacio))))


;;OBSERVADORES
;;Predicados

(define arbol-vacio?
  (lambda (n)
    (eqv? (car n) 'arbol-vacio)))

(define hoja?
  (lambda (n)
    (eqv? (car n) 'hoja)))

(define nodo?
  (lambda (n)
    (eqv? (car n) 'nodo)))

;;EXTRACTORES

(define hoja->numero
  (lambda (n)
    (cadr n)))

(define nodo->key
  (lambda (n)
    (cadr n)))

(define nodo->h1
  (lambda (n)
    (caddr n)))

(define nodo->h2
  (lambda (n)
    (cadddr n)))

(define nodo->h3
  (lambda (n)
    (car (cddddr n))))

;;;AREA DEL PROGRAMADOR

(define suma-hojas
  (lambda (arb)
    (cond
      [(arbol-vacio? arb) 0]
      [(hoja? arb) (hoja->numero arb)]
      [(nodo? arb) (+
                    (suma-hojas (nodo->h1 arb))
                    (suma-hojas (nodo->h2 arb))
                    (suma-hojas (nodo->h3 arb)))]
      [else (eopl:error "Esto no es un arbol")]
      )))

(suma-hojas arbol1)

(define arbol->lista
  (lambda (arb)
    (cond
      [(arbol-vacio? arb) '()]
      [(hoja? arb) (list (hoja->numero arb))]
      [(nodo? arb)
       (append
        (list (nodo->key arb))
        (arbol->lista (nodo->h1 arb))
        (arbol->lista (nodo->h2 arb))
        (arbol->lista (nodo->h3 arb)))]
      [else (eopl:error "Esto no es un árbol")]
      )))

(arbol->lista arbol1)