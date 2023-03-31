;Dise ̃na la funci ́on (countdown) toma un n ́umero n y genera una lista de n ́umeros natu-
;rales entre n y 0. Si el n ingresado es menor que 0 retorna una lista vac ́ıa
#lang eopl
(define countdown
  (lambda (n)
    (cond
      [(<= n 0) '()]
      [else (cons n (countdown (- n 1)))])))
;pruebas
(display (countdown 5))
(newline)
(display (countdown -3))