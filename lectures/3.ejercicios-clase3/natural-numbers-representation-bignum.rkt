#lang eopl
(define base 16)
(define zero '())
(define zero? null?)

; === Interfaz ===
(define succ
  (lambda (n)
    (cond
      [(zero? n) (list 1)]
      [(= (car n) (- base 1)) (cons 0 (succ (cdr n)))]
      [else (cons (+ 1 (car n)) (cdr n))])))
(define pred
  (lambda (n)
    (cond
      [(zero? n)(eopl:error "El número no puede ser cero")]
      [(= (car n) 0) (cons (- base 1) (pred (cdr n))]    
      [(eqv? (succ zero) n) '()]
      [else (cons (- (car n) 1) (cdr n))])))

; === Area del programador ===
(define suma
  (lambda (a b)
    (if
      (zero? b)
      a
      (succ (suma a (pred b))))))

#| Lo más importante a entender sobre la abstracción de datos, es que al final no importa tanto como los representemos ya que el usuario final (programador) podra hacer usuo de la Interfaz para trabajar los datos sin importar su represetnaiín (Notese que la función suma, es igual sin importar como rerpesentemso los datos)
