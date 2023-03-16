(define exits?
  (lambda (pred lst)
    [(null? lst) #F]
    [(pred (car lst)) #T]
    [else (exits? pred (cdr lst))]))

(display exits? number? '(a b c 5 e))
;ver ejemplo de operadores de corto circuito
(display exits? (lambda (x) (and (number? x)(> x 3)) '(a 2 3 4 5)))
