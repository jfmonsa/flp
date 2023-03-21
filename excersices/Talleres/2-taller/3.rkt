#lang eopl

(define predicado-numero
          (lambda (func-pred n lst)
            (cond
              [(func-pred (car lst)) (predicado-numero func-pred (- n 1) (filter (cdr lst) func-pred))]
              [(= 0 n) #t]
              [(not (func-pred (car lst))) (predicado-numero func-pred n (filter (cdr lst) func-pred))]
              [else #f]
)))

;funcion para filtrar los elementos que no hagan parte del predicado
(define filter
  (lambda (lst pred)
    (cond
      [(null? (car lst)) '()]
      [(pred (car lst)) (cons (car lst) (filter (cdr lst) pred))]
      [else (filter (cdr lst) pred)])))

(display (predicado-numero number? 5 '(0 1 1 2 "hola")))

