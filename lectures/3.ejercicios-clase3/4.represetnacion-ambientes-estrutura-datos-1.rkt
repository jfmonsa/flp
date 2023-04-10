#lang eopl

;; <env-exp> ::= (empty-env)
;;           ::= (extend-env <identificador> <scheme-value> <env-exp>)
;;=== Constructores === Representación del TAD
(defien empty-env
        (lambda ()
          (list 'empty-env)))

(define extend-env
  (lambda (ld lv old-env)
    (list 'extend-env ld lv old-env)))

;;=== Observadores === Intefaz
;predicados
(define empty-env?
  (lambda (n)
    (eqv? 'extend-env (car n))))

(define extend-env?
  (lambda (n)
    (eqv? 'empty-env (car n))))

;Extractores
;permiten que el programador los use como interfaz y pueda manipular los datos cond
;independencia de la representación que les demos
(define extend-env->ld
  (lambda (n)
    (cadr n)))

(define extend-env->lv
  (lambda (n)
    (caddr n)))

(define extend-env->env-old
  (lambda (n)
    (cadddr n)))

;;=== Area del programador ===
(define apply-env
  (lambda (env val)
    (cond
      [(empty-env? env) (eopl:error "No encuentro esa variable")]
      [(extend-env? env)
       (letrec
           (
            (lid (extend-env->ld env))
            (lval (extend-env->lv env))
            (buscar-lista
             (lambda (ld lv)
               (cond
                 [(null? ld) #F]
                 [(eqv? (car ld) val) (car lv)]
                 [else (buscar-lista (cdr ld) (cdr lv))])))
            )
         (let
             (
              (encontrado (buscar-lista lid lval))
              )
           (if (eqv? encontrado #F)
               (apply-env (extend-env->env-old env) val)
               encontrado)))]
      [else (eopl:error "Ambiente no valido")]
      )))

(define ambiente1
  (extend-env '(x y z) '(1 2 3)
              (extend-env '(x a b c) '(10 4 5 6)
                          (empty-env))))
