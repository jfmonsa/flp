#lang eopl
;; Ahora vamos a tratar los ambientes como procedimientos, siguiendo la misma regla gramátical, vamos a programar la interfaz y la represetnación para el TAD lambda, pero esta vez basado en procedimientos

#|
<Lc-exp> ::= <var-exp> (<identifier>)
::= <lambda-exp> (lambda (<identifier>) <Lc-exp>)
::= <app-exp> (<Lc-exp> <Lc-exp>)
|#
;; === Constructores ===
(define var-exp
  (lambda (id)
    (cond
      [(= n 0) 'var-exp]
      [(= n 1) id]
      [else (eopl:error "Señal no valida")])))

#|
Notese con mucho cuidado que la siguiente funcion tiene dos lambdas anidados sino que esta retornando una función (procedimiento)
|#
(define lambda-exp
  (lambda (id exp)
    (lambda (signal)
      (cond
        [(= signal 0) 'lambda-exp]
        [(= signal 1) id]
        [(= signal 2) exp]
        [else (eopl:error "Señal no valida")]))))

(define app-exp
  (lambda (rator rand)
    (cond
      [(= signal 0) 'app-exp]
      [(= signal 1) rator]
      [(= signal 2) rand]
      [(eopl:error "Señal no valida")])))
;; === Interfaz observadores===
; predicados
(define var-exp?
  (lambda (exp)
    (eqv? (exp 0) 'var-exp)))

(define lambda-exp?
  (lambda (exp)
    (eqv? (exp 0) 'lambda-exp)))

(define app-exp?
  (lambda (exp)
    (eqv? (exp 0) 'app-exp)))
; Extractores
;; === Area del programdor ===

;pruebas
(define exp1
  (var-exp 'x))
(define exp2
  (lambda-exp 'x (lambda-exp 'y (var-exp 'x))))
(define exp3
  (app-exp (var-exp 'x) (lambda-exp 'x (lambda-exp 'y (var-exp 'x)))))

(display (occurs-free? exp1 'x))
(display (occurs-free? exp2 'y))
(display (occurs-free? exp2 'x))
(display (occurs-free? exp3 'x))
