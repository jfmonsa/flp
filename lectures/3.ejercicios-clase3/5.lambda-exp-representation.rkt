#lang eopl
#|
Definir una expresión lambda siguiendo la siguiente regla BNF

<Lc-exp> ::= <var-exp> (<identifier>)
::= <lambda-exp> (lambda (<identifier>) <Lc-exp>)
::= <app-exp> (<Lc-exp> <Lc-exp>)
|#

;;x
;consturctores
(define var-exp
  (lambda (id)
    (list 'var-exp id)))

(define lambda-exp
  (lambda (id exp)
    (list 'lambda-exp id exp)))

(define app-exp
  (lambda (rator rand)
    (list 'app-exp rator rand)))
;=== Funciones observadoras ===
;predicados
(define var-exp?
  (lambda (exp)
    (eqv? (car exp) 'var-exp)))

(define lambda-exp?
  (lambda (exp)
   (eqv? (car exp) 'lambda-exp) ))

(define app-exp?
  (lambda (exp)
    (eqv? (car exp) 'app-exp)))
;Extractores
(define var-exp->id
  (lambda (exp)
    (cadr exp)))

(define lambda-exp->id
  (lambda (exp)
    (cadr exp)))

(define lambda-exp->exp
  (lambda (exp)
    (caddr exp)))

(define app-exp->rator
  (lambda (exp)
    (cadr exp)))

(define app-exp->rand
  (lambda (exp)
    (caddr exp)))
; === Area del programador ===
;función para saber si una variable ocurre libre en el lambda
(define occurs-free?
  (lambda (exp var)
    (cond
      [(var-exp? exp) (eqv? var (var-exp->id exp))]
      [(lambda-exp? exp)(and
                          (not (eqv? var (lambda-exp->id exp)))
                          (occurs-free? (lambda-exp->exp exp) var))]
      [(app-exp? exp) (or
                        (occurs-free? (app-exp->rand exp) var)
                        (occurs-free? (app-exp->rator exp) var))]
      [else (eopl:error "Dato no valido")])))
;Como podemos ver la función la contruimos usando la interfaz, sin importarnos como esta representado internamente el dato (expresión lambda)

;Veamos como se diferencia entonces de la siguiente función la cual no hace uso de interfaces, sino que evalua la expresión con car y cdr
#|
#lang eopl
(define ocurre-libre
  (lambda (exp var)
    (cond
      [(symbol? exp) (eqv? exp var)]
      [(eqv? (car exp) 'lambda)
       (and
        (not (eqv? var (caadr exp)))
        (ocurre-libre (caddr exp) var)
        )
       ]
      [else
       (or
        (ocurre-libre (car exp) var)
        (ocurre-libre (cadr exp) var))

       ]
      )
    )
  )

(display (ocurre-libre 'x 'x)) (newline)
(display (ocurre-libre '(x (lambda (x) (x x))) 'x)) (newline)
(display (ocurre-libre '((lambda (x) (x x)) (lambda (y) (lambda (x) (x y )))) 'x)) (newline)
|#

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
