#lang eopl
; === Representación ===
(define zero '())
; === Representación ===

; === Intefaz ===
(define zero?
  (lambda (n)
    (equal? n zero)))

(define pred
  (lambda (n)
    (if
      (equal? n zero)
      (eopl:error "No se puede tener predecesor de zero")
      (cdr n))))

(define succ
  (lambda (n)
    (cons #t n)))
; === Intefaz ===

; === Area del programador ===
(define suma
  (lambda (a b)
    (if
      (zero? b)
      a
      (succ (suma a (pred b))))))

(display (suma 
      (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
      (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t
            #t #t #t #t #t #t #t #t #t #t #t #t #t #t
            #t #t #t #t #t #t #t #t #t #t #t #t #t #t)))
