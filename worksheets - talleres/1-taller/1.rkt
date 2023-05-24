#lang eopl
;*Juan Felipe Monsalve Vargas - 202160145 - juan.felipe.monsalve@correounivalle.edu.co 
;*Juan Pablo Idárraga Pabón - 202160114 - idarraga.juan@correounivalle.edu.co
;*Jose Luis Ramos Arango - 202159889 - jose.luis.ramos@correounivalle.edu.co 
;funciones
(define predicado-legendario
  (lambda (lst-pred lst-elem)
   (cond
    [(or (null? lst-pred) (null? lst-elem)) '()]
    [else
     (cons (cumplir-pred (car lst-pred) lst-elem);genera la lista con el primer enunciado
     (predicado-legendario (cdr lst-pred) lst-elem))]
    ))) ;llamado recursivo para hacer las otras listas 
         
(define cumplir-pred
 (lambda (pred lst) 
  (cond
   [(null? lst) '()]
   [(list? (car lst))
    (cons
     (cumplir-pred pred (car lst))
     (cumplir-pred pred (cdr lst)))]
   [(pred (car lst))
     (cons
      (car lst)
      (cumplir-pred pred (cdr lst)))]
   [else (cumplir-pred pred (cdr lst))]))) ;elimina el elemento que no cumple el predicado

;datos de prueba
(define l1 (list number? symbol? (lambda (x) (and (number? x)
                                                  (> x 3)))
                 (lambda (x) (and (number? x)
                                  (<= x 4)))))
(define l2 '(1 2 3 (a b) (2 4 x y z) 5 6 7 (1 (a b 3) 4)))
;prueba
;(display (cumplir-pred (caddr l1) l2))
(display(predicado-legendario l1 l2))