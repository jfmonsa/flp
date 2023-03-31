;Dise ̃na la funci ́on (binary-to-natural) recibe una lista con 0 o 1 que representan un
;n ́umero binario. Esta funci ́on opera recursivamente la lista para obtener el n ́umero decimal
;equivalente
#lang eopl
(define binary-to-natural
  (lambda (lst [exponent 0])
    (cond
      [(null? lst) 0]
      [else
       (if (= 1 (car lst))
        (+ (binary-to-natural (cdr lst) (+ exponent 1)) (expt 2 exponent))
        (binary-to-natural (cdr lst) (+ exponent 1))
       )]
      )))

(display (binary-to-natural '()))
(newline)

(display (binary-to-natural '(1 0 0)))
(newline)

(display (binary-to-natural '(1 1 0 0)))
(newline)

(display (binary-to-natural '(1 1 1 1)))
(newline)

(display (binary-to-natural '(1 0 1 0 1)))
(newline)

(display (binary-to-natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)))
(newline)