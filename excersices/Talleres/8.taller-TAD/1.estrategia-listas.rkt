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
;; 1. get-numbers: Retorna una lista con todos los n ́umeros que se encuentran dentro de la estructura,
;; esta lista debe ser plana
(define get-numbers
  (lambda (exp)
    (cond
      [(empty-record? exp) '()]
      [(non-empty-record? exp) ;llamado recursivo (registro)
       (append
               (get-numbers (non-empty-record->elm exp)) ;(1)
               (get-numbers (non-empty-record->record exp)))]
      ;En caso de obtener un elemento (1) dos casos posibles simple-item? o list-item?
      ;simple-item -> si es un item que tiene como hijo un número
      [(and (simple-item? exp) (item-num? (simple-item->datum exp)))
       (list (item-num->datum (simple-item->datum exp)))
       ]
      ;list-item -> si es un list-item que tiene como primer elemento una lista no vacia
      [(list-item? exp)
       (get-numbers (list-item->datum exp))]
      [(non-empty-list-item? exp)
       (append (get-numbers (non-empty-list-item->elm exp))
               (get-numbers (non-empty-list-item->lst exp)))]
      ;En otro caso
      [else '()]
      )))

;pruebas
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
;; 2. get-symbols: Retorna una lista con todos los s ́ımbolos que se encuentran dentro de la estructura,
;; esta lista debe ser plan
(define get-symbol
  (lambda (exp)
    (cond
      [(empty-record? exp) '()]
      [(non-empty-record? exp) ;llamado recursivo (registro)
       (append
        ;identificador del record debe agregarse a la lista de resultado
        (list (non-empty-record->key exp))
        (get-symbol (non-empty-record->elm exp)) ;(1)
        (get-symbol (non-empty-record->record exp)))
       ]
      ;En caso de obtener un elemento (1) dos casos posibles simple-item? o list-item?
      ;simple-item -> si es un item que tiene como hijo un número 
;      [(simple-item? exp) 
;       (get-symbol (simple-item->datum exp))]
;      [(item-sym? exp) (list (item-sym->datum exp))]
      [(and (simple-item? exp) (item-sym? (simple-item->datum exp)))
       (list (item-sym->datum (simple-item->datum exp)))]
      ;list-item -> si es un list-item que tiene como primer elemento una lista no vacia
      [(list-item? exp) (get-symbol (list-item->datum exp))]
      [(non-empty-list-item? exp)
       (append
        (get-symbol (non-empty-list-item->elm exp))
        (get-symbol (non-empty-list-item->lst exp)))]
      ;En otro caso
      [else '()]
      )))
;pruebas
(newline)
(display (get-symbol reg1))