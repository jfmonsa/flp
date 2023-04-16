;Autores: Jose Luis Ramos Arango - 2159889; Juan Pablo Idarraga - 2160114; Juan Felipe Monsalve -2160145;
;Leider Cortes Hernandez - 2159879

#lang eopl
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
; Un constructor por cada case
(define empty-record
  (lambda ()
    (lambda (s)
      (cond
        [(= s 0) 'empty-record]
        [else (eopl:error "Señal invalida en arbol-vacio")]
    ))))

(define non-empty-record
  (lambda (key elm rec)
    (lambda (s)
      (cond
        [(= s 0) 'non-empty-record]
        [(= s 1) key]
        [(= s 2) elm]
        [(= s 3) rec]
        [else (eopl:error "Señal invalida en arbol-vacio")]
    ))))


;;Datatype elem (item)
(define simple-item
  (lambda (dat)
    (lambda (s)
      (cond
        [(= s 0) 'simple-item]
        [(= s 1) dat]
        [else (eopl:error "Señal invalida en arbol-vacio")]
        ))))

(define list-item
  (lambda  (dat)
    (lambda (s)
      (cond
        [(= s 0) 'list-item]
        [(= s 1) dat]
        [else (eopl:error "Señal invalida en arbol-vacio")]
        ))))

;;Datatype simple-item
(define item-num
  (lambda (dat)
    (lambda (s)
      (cond
        [(= s 0) 'item-num]
        [(= s 1) dat]
        [else (eopl:error "Señal invalida en arbol-vacio")]
        )
      )))

(define item-sym
  (lambda (dat)
    (lambda (s)
      (cond
        [(= s 0) 'item-sym]
        [(= s 1) dat]
        [else (eopl:error "Señal invalida en arbol-vacio")]
        ))))

;;Datatype-list item
(define empty-list-item
  (lambda ()
    (lambda (s)
      (cond
        [(= s 0) 'empty-list-item]
        [else (eopl:error "Señal invalida en arbol-vacio")]
        ))))

(define non-empty-list-item
  (lambda (elm lst)
    (lambda (s)
      (cond
        [(= s 0) 'non-empty-list-item]
        [(= s 1) elm]
        [(= s 2) lst]
        [else (eopl:error "Señal invalida en arbol-vacio")]
        ))))
;;========== Predicados ==========
; Contar elementos
(define empty-record?
  (lambda (n)
    (= (n 0) 'empty-record)))
(define non-empty-record?
  (lambda (n)
    (= (n 0) 'non-empty-record)))
(define simple-item?
  (lambda (n)
    (= (n 0) 'simple-item)))
(define list-item?
  (lambda (n)
    (= (n 0) 'list-item)))
(define item-num?
  (lambda (n)
    (= (n 0) 'item-num)))
(define item-sym?
  (lambda (n)
    (= (n 0) 'item-sym)))
(define empty-list-item?
  (lambda (n)
    (= (n 0) 'empty-list-item)))
(define non-empty-list-item?
  (lambda (n)
    (= (n 0) 'non-empty-list-item)))
;; ====== Extractores ========
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
(define hoja->numero
  (lambda (n)
    (n 1)))

;; Nota: los elementos vacios (empty-*) no tienen extractor
;<record>
(define non-empty-record->key
  (lambda (n)
    (n 1)))
(define non-empty-record->elm
  (lambda (n)
    (n 2)))
(define non-empty-record->record
  (lambda (n)
    (n 3)))

;<element>
(define simple-item->datum
  (lambda (n)
    (n 1)))
(define list-item->datum
  (lambda (n)
    (n 1)))

;<simple-item>
(define item-num->datum
  (lambda (n)
    (n 1)))
(define item-sym->datum
  (lambda (n)
    (n 1)))

;<list-item>
(define non-empty-list-item->elm
  (lambda (n)
    (n 1)))
(define non-empty-list-item->lst
  (lambda (n)
    (n 2)))

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
