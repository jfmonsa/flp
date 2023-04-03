#lang eopl
;======= Representación =======
(define zero 0)

;predicado
(define zero?
  (lambda (n)
    (= n 0)))

;======= Representación =======

;======= Interfaz =======
;definiendo función predecesora
(define pred
  (lambda (n)
    (if
      (zero? n)
      (eopl:error "No se puede sacar el predesesor de cero")
      (- n 1))))

;defininedo el sucesor
(define succ
  (lambda (n)
    (+ n 1)))
;======= Interfaz =======
;======= Implementación =======
;el programador usa los procediminetos de la interfaz
(define suma
  (lambda (a b)
    (if
      (zero? b)
      a
      (succ (suma a (pred b))))))

(display (suma 5 4))
