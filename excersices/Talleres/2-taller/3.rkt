#lang eopl
#|
Dise ̃na la funci ́on predicado-numero esta recibe una funci ́on predicado f , un n ́umero n
y una lista l. Se retorna #t si existen exactamente n elementos en la lista l que cumnplen
con el predicado f , ejemplo
|#

(define predicado-numero
  (lambda (func-pred n lst)
    (if (= n (lenght-list (filtro func-pred lst)))
      #T
      #F
    )))

(define filtro
  (lambda (func-pred lst)
    (cond
      [(null? lst) '()]
      [(func-pred (car lst))
       (cons (car lst) (filtro func-pred (cdr lst)))]
      [else
       (filtro func-pred (cdr lst))])))

(display (filtro number? '(0 0 1 1 2 "hola" )))
(newline)

(define lenght-list
  (lambda (lst)
      (if (null? lst)
          0
          (+ 1 (lenght-list (cdr lst))))))

(display (lenght-list '(1 2 3)))
(newline)

;pruebas
(display (predicado-numero number? 5 '( 0 0 1 1 2 "hola")))
(newline)
(display (predicado-numero number? 4 '( 0 0 1 1 2 "hola" )))