#lang eopl
;2. Dada la siguiente gram ́atica de una lista de pares
;  <pair-lst> ::=(vacia) ’()
;             ::=(novacia) <pair> <pair-lst>
;  <pair> ::=(par) <int> <int>

;función auxiliar
(define contar-elementos
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (contar-elementos (cdr lst))))))
;Realice las funciones
;  a) in-S? (determinar si un elemento pertenece a esta gram ́atica)
;constructores
(define vacia
  (lambda ()
    '()))
(define par
  (lambda (int1 int2)
    (if (and (integer? int1) (integer? int2))
        (list 'par int1 int2)
        (eopl:error "Contract violation, arguments must be integers"))))
(define novacia
  (lambda (pair pair-lst)
    (if (and (par? pair) (pair-lst? pair-lst))
        (list 'novacia pair pair-lst)
        (eopl:error "Contract violation")
        )))
                                
;predicados
(define vacia? null?)

(define novacia?
  (lambda (exp)
    (and
     (list? exp)
     (not (null? exp))
     (not (null? (cdr exp)))
     (eqv? (car exp) 'novacia)
     (pair? (cadr exp))
     (pair-lst? (caddr exp)))))
   
(define par?
  (lambda (exp)
    (and
     (list? exp)
     (= (contar-elementos exp) 3)
     (eqv? (car exp) 'par)
     (integer? (cadr exp))
     (integer? (caddr exp)))))

(define pair-lst? ;in-S?
  (lambda (exp)
    (or
     (vacia? exp)
     (novacia? exp))))
;pruebas
(display (par? (par 23 42)))
(newline)
(display (novacia? (novacia (par 23 55) (novacia (par 18 31) (vacia)))))
(newline)
(display (pair-lst? (vacia)))
(newline)
(display (pair-lst? (novacia (par 23 11) (novacia (par 12 7534) (novacia (par 3213 90) (vacia)))))) 

;  b) buscar-numero: Retorna verdadero si el n ́umero est ́a presente en la lista de pares
;extractores
(define pair->v1
  (lambda (exp)
    (if (par? exp)
        (cadr exp)
        (eopl:error "Contract violation"))))

(define pair->v2
  (lambda (exp)
    (if (par? exp)
        (caddr exp)
        (eopl:error "Contract violation"))))

(define novacia->primer-par
  (lambda (exp)
    (if (novacia? exp)
        (cadr exp)
         (eopl:error "Contract violation"))))

(define novacia->segundo-par
  (lambda (exp)
    (if (novacia? exp)
        (caddr exp)
         (eopl:error "Contract violation"))))

(define buscar-numero
  (lambda (num lst-pair)
    (cond
      [(not (and (integer? num) (pair-lst? lst-pair)))
       (eopl:error "Contract vilation")]
      [(vacia? lst-pair) #f]
      [(or (= (pair->v2 (novacia->primer-par lst-pair)) num)
           (= (pair->v1 (novacia->primer-par lst-pair)) num)) #t]
      [else (buscar-numero num (novacia->segundo-par lst-pair))]
     )))
;pruebas
(newline)
(display (buscar-numero 88
     ;lista para buscar
    (novacia (par 23 11) (novacia (par 12 7534) (novacia (par 3213 90) (novacia (par 44 55) (novacia (par 33 88) (vacia))))))))
                        
;  c) sumar-pares, esta funci ́on recibe una lista de pares y retorna un par que representa
;la suma de cada uno de los elementos. Ejemplo ((1 2) (2 3)(3 4)) retorna (6 9), ’()
;retorna (0 0)
(define sumar-pares
  ;par-acom: Acomula un par que lleva la suma parcial
  (lambda (lst-pair [par-acom (par 0 0)])
    (cond
      [(not (pair-lst? lst-pair)) (eopl:error "Contract vilation")]
      [(vacia? lst-pair)
       ;si se pasa la lista vacia como argumetno lst-pair, la función retorna: (par 0 0)
       (par
        (pair->v1 par-acom)
        (pair->v2 par-acom))
       ]
      [else
       ;Como estamos haciendo uso de un acomulador estamos utilizando recursión de cola
       ;llamado recursivo:
       (sumar-pares
        ;Invoco la función pero ahora  lst-pair va ser el resto de la lista
        (novacia->segundo-par lst-pair)
        ;En el acomulador envio la suma parcial
        (par
         (+ (pair->v1 (novacia->primer-par lst-pair)) (pair->v1 par-acom))
         (+ (pair->v2 (novacia->primer-par lst-pair)) (pair->v2 par-acom))
         ))]
      )))
;Si hubieramos querido usar recursión de cabeza, el cuerpo de nuestro else sería:
;       (par
;        ;Sumar el primer valor del par
;        (+ (pair->v1 (novacia->primer-par lst-pair))
;           (pair->v1 (sumar-pares (novacia->resto lst-pair))))
;        ;Sumar el segundo valor del par
;        (+ (pair->v2 (novacia->primer-par lst-pair))
;           (pair->v2 (sumar-pares (novacia->resto lst-pair)))))]
;      )))

;Nota: También hubieramos podido hacer uso de estructuras como let, let* y letrec
;pruebas
(newline)
;() , retorna (0 0)
(display (sumar-pares (vacia)))
(newline)
;((1 2) (2 3)(3 4)) retorna (6 9)
(display (sumar-pares
          (novacia (par 1 2) (novacia (par 2 3) (novacia (par 3 4) (vacia))))
          ))