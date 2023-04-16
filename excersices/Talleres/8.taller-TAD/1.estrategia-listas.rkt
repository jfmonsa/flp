;Autores: Jose Luis Ramos Arango - 2159889; Juan Pablo Idarraga - 2160114; Juan Felipe Monsalve -2160145;
;Leider Cortes Hernandez - 2159879

#lang eopl
;<record>
;  ::=’() empty-record
;  ::= <symbol> <element> <record>  non-empty-record(key, elm, record)
;
;<element>
;  ::=<simple-item> simple-item(datum)
;  ::=<list-item> list-item(datum)
;
;<simple-item>
;   ::= <number>
;     item-num(datum)
;   ::=<symbol>
;     item-sym(dtaum)
;
;<list-item>
;    ::= ’()
;       empty-list-item()
;    ::=  <simple-item> <list-item>
;       non-empty-list-item(elm, lst)
;;============ Constructores ===============
;Datatype record
(define empty-record
  (lambda ()
    (list 'empty-record)))
(define non-empty-record
  (lambda (key elm rec)
    (list 'non-empty-record key elm rec)))

;;Datatype elem (item)
(define simple-item
  (lambda (dat)
    (list 'simple-item dat)))

(define list-item
  (lambda (dat)
    (list 'list-item dat)))

;;Datatype simple-item
(define item-num
  (lambda (dat)
    (list 'item-num dat)))

(define item-sym
  (lambda (dat)
    (list 'item-sym dat)))

;;Datatype-list item
(define empty-list-item
  (lambda ()
    (list 'empty-list-item)))

(define non-empty-list-item
  (lambda (elm lst)
    (list 'non-empty-list-item elm lst)))

;;========== Predicados ==========
; Contar elementos
(define aux-contar-elm
  (lambda (exp)
    (cond
      [(null? exp) 0]
      [(+ 1 (aux-contar-elm (cdr exp)))])))

(define empty-record?
  (lambda (exp)
    (and
     (list? exp)
     (= 1 (aux-contar-elm exp))
     (eqv? (car exp) 'empty-record))))

  
(define non-empty-record?
  (lambda (exp)
    (and
      (list? exp)
      (= (aux-contar-elm exp) 4)
      (eqv? (car exp) 'non-empty-record))))
;pruebas
;(display (empty-record? (empty-record))) ;t
;(newline)
;(display (empty-record? (non-empty-record 'y 6 (empty-record)))) ;f
;(newline)
;(display (non-empty-record? (non-empty-record 'y 6 (empty-record)))) ;t

(define simple-item?
  (lambda (exp)
    (and
      (list? exp)
      (= (aux-contar-elm exp) 2)
      (eqv? (car exp) 'simple-item ))))

(define list-item?
  (lambda (exp)
    (and
      (list? exp)
      (= (aux-contar-elm exp) 2)
      (eqv? (car exp) 'list-item))))

(define item-num?
  (lambda (exp)
    (and
      (list? exp)
      (= (aux-contar-elm exp) 2)
      (eqv? (car exp) 'item-num))))

(define item-sym?
  (lambda (exp)
    (and
      (list? exp)
      (= (aux-contar-elm exp) 2)
      (eqv? (car exp) 'item-sym))))

(define empty-list-item?
  (lambda (exp)
    (and
      (list? exp)
      (= (aux-contar-elm exp) 1)
      (eqv? (car exp) 'empty-list-item))))

(define non-empty-list-item?
  (lambda (exp)
    (and
      (list? exp)
      (= (aux-contar-elm exp) 3)
      (eqv? (car exp) 'non-empty-list-item))))

;; ====== Extractores ========

;; Nota: los elementos vacios (empty-*) no tienen extractor
;<record>
(define non-empty-record->key
  (lambda (exp)
    (cadr exp)))
(define non-empty-record->elm
  (lambda (exp)
    (caddr exp)))
(define non-empty-record->record
  (lambda (exp)
    (cadddr exp)))

;<element>
(define simple-item->datum
  (lambda (exp)
    (cadr exp)))
(define list-item->datum
  (lambda (exp)
    (cadr exp)))

;<simple-item>
(define item-num->datum
  (lambda (exp)
    (cadr exp)))
(define item-sym->datum
  (lambda (exp)
    (cadr exp)))

;<list-item>
(define non-empty-list-item->elm
  (lambda (exp)
    (cadr exp)))
(define non-empty-list-item->lst
  (lambda (exp)
    (caddr exp)))
;; ===== Area del programdor ====
;;ise ̃ne las siguientes funciones para cada representaci ́on:
;; 1. get-numbers: Retorna una lista con todos los n ́umeros que se encuentran dentro de la estructura,
;; esta lista debe ser plana
;; 2. get-symbols: Retorna una lista con todos los s ́ımbolos que se encuentran dentro de la estructura,
;; esta lista debe ser plan


;<record>
;  ::=’() empty-record
;  ::= <symbol> <element> <record>  non-empty-record(key, elm, record)
;
;<element>
;  ::=<simple-item> simple-item(datum)
;  ::=<list-item> list-item(datum)
;
;<simple-item>
;   ::= <number>
;     item-num(datum)
;   ::=<symbol>
;     item-sym(dtaum)
;
;<list-item>
;    ::= ’()
;       empty-list-item()
;    ::=  <simple-item> <list-item>
;       non-empty-list-item(elm, lst)
;<element>
;(define simple-item->datum
;  (lambda (exp)
;    (cadr exp)))
;(define list-item->datum
;  (lambda (exp)
;    (cadr exp)))
;
;;<simple-item>
;(define item-num->datum
;  (lambda (exp)
;    (cadr exp)))
;(define item-sym->datum
;  (lambda (exp)
;    (cadr exp)))
;
;;<list-item>
;
;(define non-empty-list-item->elm
;  (lambda (exp)
;    (cadr exp)))
;(define non-empty-list-item->lst
;  (lambda (exp)
;    (caddr exp)))
;(list-item
;     (non-empty-list-item
;      (simple-item (item-sym 'x))
;      (non-empty-list-item
;       (simple-item (item-num 4))
;       (empty-list-item))))
;00000000000000
(define get-numbers
  (lambda (exp)
    (cond
      [(empty-record? exp) '()]
      [(non-empty-record? exp) ;llamado recursivo (registro)
      (cons 
        (get-numbers (non-empty-record->elm exp))
        (get-numbers (non-empty-record->record exp)))]
      
      [(and (simple-item? exp) (number? (item-num->datum (simple-item->datum exp))))
            (item-num->datum (simple-item->datum exp))]
      [(list-item? exp);;Llamado recursivo (listas)
       (cons
        (get-numbers (non-empty-list-item->elm (list-item->datum exp)))
        (get-numbers (non-empty-list-item->lst (list-item->datum exp))))]
      [else '()]
      )))

(define get-numbers-lista
  (lambda (exp)
    (cond
      [(empt
;pruebas
(define exp
  (list-item
     (non-empty-list-item
      (simple-item (item-sym 'x))
      (non-empty-list-item
       (simple-item (item-num 4))
       (empty-list-item)))))

(define reg1
  (non-empty-record
   'a
   (simple-item (item-num 4))
   (non-empty-record
    'b
    (list-item
     (non-empty-list-item
      (simple-item (item-sym 'x))
      (non-empty-list-item
       (simple-item (item-num 4))
       (empty-list-item))))
    (empty-record)
    )))

(display (get-numbers reg1))





