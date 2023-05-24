#lang eopl
;(list-set ’(a b c d) 2 ’(1 2))
(define list-set 
  (lambda (lst n r)
    (cond
      [(null? lst) '()]
      (if
        (= (index-of lst) n)
        (cons r '())
        (cons (car lst) (list-set (cdr lst) n r))
        )
      )))




