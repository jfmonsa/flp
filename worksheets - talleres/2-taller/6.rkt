#lang eopl
(define natural-to-binary
  (lambda (n)
    (cond
      [(= n 0) 0]
      [(<= n 2) 1]
      [else
       (cons
        (natural-to-binary (quotient n 2))
        (modulo n 2)
        )])))
;pruebas
(display (natural-to-binary 0))
(newline)
(display (natural-to-binary 4))
(newline)
(display (natural-to-binary 12))
(newline)
(display (natural-to-binary 15))
(newline)
(display (natural-to-binary 21))
(newline)
(display (natural-to-binary 8191))
(newline)