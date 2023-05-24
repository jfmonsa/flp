#lang eopl
(define a 5)
(define b (list 1 2 3 4))
(define c #t)
(define d "hola")

(* (+ 5 2) (* 2 3))

; Solamente ilustrativo, vamos a procurar usar en el curso funciones anonimas
; la ventaja de las funciones anonimas es que son las a diferencia de una función
; normal, estas las podremos pasar como argumento a otra función para que la otra función
; la invoque en su interior

;Mientras que si intentamos lo anterior con una función normal, la función se ejecutará
;en el momento en que la pasemos como argumento y no podrá ser utilizada por la otra función
(define (function1 x y)
  (+ x y))

(define (func4 a b)
  (if
   (> a b)
   a
   b))

;Funciones lambda
;a continucación vamos a asignarle funciones lambda a una "variable"
;sintaxis para python: a2 = lambda x,y: x+y

(define a2 (lambda x y)(+ x y))

(list 1 2 3) ;lista normal
'(a b c) ;lista simbolica

;definición de una lista
(define l1 '(1 2 (a b) (1 2 3) (2 3 2)))

;Acceder a los elementos de una lista
(car l1) ;-> devuelve elemento de l1

;nota: cuando utilizamos #lang opl, (first ...) y todo ese tipo de funciones no existen
(cdr l1) ;-> devuelve el resto de l1

;combinando car y cdr puedo acceder a elementos especificos de la lista reemplazando el uso de funciones como (first ...)
(cadr l1) ; devuelve el segundo elemento de l1
;para acceder al tercer elemento
(caddr l1) ;que es lo mismo que (car (cdr (cdr l1)))

;ejercicio básico de recursion
(define sumar-lista
  (lambda (lst)
    (cond
      [(null? lst) 0]
      [else
        (+ (car lst) (sumar-lista (cdr lst)))]))
