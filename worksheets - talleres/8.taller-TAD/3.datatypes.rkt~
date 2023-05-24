#lang eopl
;Autores: Jose Luis Ramos Arango - 2159889; Juan Pablo Idarraga - 2160114; Juan Felipe Monsalve -2160145;
;Leider Cortes Hernandez - 2159879

;<record>
;  ::=’() empty-record
;  ::= <symbol> <element> <record>  non-empty-record(key, elm, record)

;<element>
;  ::=<simple-item> simple-item(datum)
;  ::=<list-item> list-item(datum)

;<simple-item>
;   ::= <number>
;     item-num(datum)
;   ::=<symbol>
;     item-sym(dtaum)

;<list-item>
;    ::= ’()
;       empty-list-item()
;    ::=  <simple-item> <list-item>
;       non-empty-list-item(elm, lst)

;;============ Constructores ===============
;;Datatypes

(define-datatype record record?
  (empty-record)
  (non-empty-record (key symbol?)
                    (elm element?)
                    (rec record?)))

(define-datatype element element?
  (simple-item-e (datum simple-item?))
  (list-item-e (datm list-item?)))

(define-datatype simple-item simple-item?
  (item-num (num number?))
  (item-sym (sym symbol?)))

(define-datatype list-item list-item?
  (empty-list-item)
  (non-empty-list-item (simple-item-e simple-item?)
                       (list-item-e list-item?)))


;; ===== Area del programdor ====
;; 1. get-numbers: Retorna una lista con todos los n ́umeros que se encuentran dentro de la estructura,
;; esta lista debe ser plana

(define get-numbers
  (lambda (exp)
    (cond
      [(record? exp)
       (cases record exp
         (empty-record () '())
         (non-empty-record (key elem rec) ;llamado recursivo (registro)
                            (append
                             (get-numbers elem) ;(1)
                             (get-numbers rec))))]
      ;En caso de obtener un elemento (1) dos casos posibles simple-item? o list-item?
      [(element? exp)
       (cases element exp
         (simple-item-e (datum) (get-numbers datum))
         (list-item-e (datm) (get-numbers datm)))
       ] 
      ;simple-item -> si es un item que tiene como hijo un número
      [(simple-item? exp)
       (cases simple-item exp
         (item-num (n) (list n))
         (else '()))]
           
      ;list-item -> si es un list-item que tiene como primer elemento una lista no vacia
      [(list-item? exp)
       (cases list-item exp
         (empty-list-item () '())
         (non-empty-list-item (simple-item-e list-item-e)
                              (append
                               (get-numbers simple-item-e)
                               (get-numbers list-item-e))))]
      [else '()]
      )))


;pruebas
(define reg1
  (non-empty-record 'rr
                    (simple-item-e (item-sym 'jha))
                    (non-empty-record 'ffgag
                                      (simple-item-e (item-num 5))
                                      (non-empty-record 'h (simple-item-e
                                                            (item-num 4)) (empty-record )))))
(define reg2 (empty-record))
(display (get-numbers reg1))
(newline)
(display (get-numbers reg2))
(newline)

;; 2. get-symbols: Retorna una lista con todos los s ́ımbolos que se encuentran dentro de la estructura,
;; esta lista debe ser plan
(define get-symbols
  (lambda (exp)
    (cond
      [(record? exp)
       (cases record exp
         (empty-record () '())
         (non-empty-record (key elem rec) ;llamado recursivo (registro)
                            (append
                             (list key)
                             (get-symbols elem) ;(1)
                             (get-symbols rec))))]
      ;En caso de obtener un elemento (1) dos casos posibles simple-item? o list-item?
      [(element? exp)
       (cases element exp
         (simple-item-e (datum) (get-symbols datum))
         (list-item-e (datm) (get-symbols datm)))
       ] 
      ;simple-item -> si es un item que tiene como hijo un número
      [(simple-item? exp)
       (cases simple-item exp
         (item-sym (n) (list n))
         (else '()))]
           
      ;list-item -> si es un list-item que tiene como primer elemento una lista no vacia
      [(list-item? exp)
       (cases list-item exp
         (empty-list-item () '())
         (non-empty-list-item (simple-item-e list-item-e)
                              (append
                               (get-symbols simple-item-e)
                               (get-symbols list-item-e))))]
      [else '()]
      )))
;pruebas
(display (get-symbols reg1))
(newline)
(display (get-symbols reg2))