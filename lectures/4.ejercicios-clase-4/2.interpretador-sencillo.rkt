#lang eopl
(define lex-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("%" (arbno  (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "?" "$"))) symbol)
    (num (digit (arbno digit)) number)
    (num ("-" digit (arbno digit)) number)
    (num (digit (arbno digit) "." digit (arbno digit)) number)
    (num ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

(define gramatical-spec
  '(
    (program (expression) a-program)
    (expression (num) lit-exp)
    (expression (identifier) var-exp)
    ;Conditionals
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    ;local bindings (ligaduras)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    ;;Primitives
    (expression (primitive "(" (separated-list expression ",") ")") prim-exp)
    (primitive ("+") sum-prim)
    (primitive ("-") minus-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("add1") add-prim)
    (primitive ("sub1") sub-prim)
    ;;primitieas booleans
    (primitive (">") greater-prim)
    (primitive (">=") greater-or-equal-prim)
    (primitive ("<") lesser-prim)
    (primitive ("<=") lesser-or-equal-prim)
    (primitive ("==") equal-prim)
    ))

;Generate automatically the datatypes
(sllgen:make-define-datatypes lex-spec gramatical-spec)

;Function to evaluate the program
(define evaluate-prog
  (lambda (prog)
    (cases program prog
           (a-program (exp) (evaluate-expression exp initial-enviroment)))))

(define evaluate-expression
  (lambda (exp env) ;expression & enviroment
    (cases expression exp
      (lit-exp (num) num)
      (var-exp (id) (apply-env env id))
      ;Boolean expressions
      (true-exp () #true)
      (false-exp () #false)
      ;Primitives expressions
      (prim-exp (prim args)
        (let
          (
           (list-of-numbers (map (lambda (x) (evaluate-expression x env)) args))
           )
          (evaluate-primitives prim list-of-numbers)
        )
      )
      ;conditionals
      (if-exp (condition do-if-true do-if-false)
        (if
          (evaluate-expression condition env) ;Evaluate the condition 
          (evaluate-expression do-if-true env) ;In case of true condition
          (evaluate-expression do-if-false env) ;In case of false condition
          )
      )
      ;Local bindings
      (let-exp (ids rands body)
               (let
                   (
                    (lvalues (map (lambda (x) (evaluate-expression x env)) rands))
                    )
                 (evaluate-expression body (extended-env ids lvalues env))
                 )
               )
    )))

;Evaluate bindings an its enviroments
(define-datatype enviroment enviroment?
  (empty-env)
  (extended-env
    (lids (list-of symbol?))
    (lvals (list-of value?))
    (old-env enviroment?)))

(define value?
  (lambda (x)
    #true))

(define apply-env
  (lambda (env var)
    (cases enviroment env
      (empty-env () (eopl:error "variable" var "not found"))
      (extended-env (lid lval old-env)
        (letrec
          (
           (search-variable (lambda (lid lval old-env)
              (cond
                [(null? lid) (apply-env old-env var)]
                [(equal? (car lid) var) (car lval)]
                [else
                  (search-variable (cdr lid) (cdr lval) old-env)]
                )))
           )
          (search-variable lid lval old-env)
           ))
      )))

;initial-enviroment
(define initial-enviroment
  (extended-env '(x y z) '(4 2 5)
    (extended-env '(a b c) '(4 5 6) (empty-env))))
;Evaluate primitives
(define evaluate-primitives
  (lambda (prim lval) ;Primitives and a list with numerical values to apply the primitives
    (cases primitive prim
      (sum-prim () (operate-prim lval + 0))
      (minus-prim () (operate-prim lval - 0))
      (mult-prim () (operate-prim lval * 1))
      (div-prim () (operate-prim lval / 1))
      (add-prim () (+ (car lval) 1))
      (sub-prim () (- (car lval) 1))
      (greater-prim () (> (car lval) (cadr lval)))
      (greater-or-equal-prim () (>= (car lval) (cadr lval)))
      (lesser-prim () (< (car lval) (cadr lval)))
      (lesser-or-equal-prim () (<= (car lval) (cadr lval)))
      (equal-prim () (= (car lval) (cadr lval)))
      )
    ))
(define operate-prim
  (lambda (lval op term) ;list of numerical values, operation and the termination {0,1}
    (cond
      [(null? lval) term]
      [else
        (op
          (car lval)
          (operate-prim (cdr lval) op term))])
    ))
;Enviroment
;Interpreter
(define interpreter
  (sllgen:make-rep-loop
   "easy-lang: $"
   evaluate-prog
   (sllgen:make-stream-parser lex-spec gramatical-spec)))

(interpreter)