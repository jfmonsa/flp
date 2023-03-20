#lang eopl
(define list-fact
  (lambda (n)
    (cond
      [(<= n 0) '()]
      [else (cons 
              (list-fact (- n 1))
              (factorial n))]
    )))

(define factorial 
  (lambda (n)
    (cond
      [(<= n 1) 1]
      [else (* n (factorial (- n 1)))]
    )))

(display (list-fact 5))