#lang eopl
(define fib
  (lambda (n)
    (cond
      [(< n 0) 0]
      [(or (= n 1) (= n 0)) n]
      [(> n 1)
        (+ (fib (- n 1)) (fib (- n 2)))]
    )))

(define lista-fib
  (lambda (n)
    (cond
      [(<= n 0) '()]
      [else
       (cons
        (lista-fib (- n 1))
        (fib n))])))

(display (lista-fib 5))
