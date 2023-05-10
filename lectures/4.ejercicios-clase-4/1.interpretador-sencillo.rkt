#lang eopl
;; Especificación lexica

#| (define-datatype declaration declaration? |#
#|   (decl-1 (a declaration?) |#
#|           (b declaration?)) |#
#|   (decl-2 (exp exp?) |#
#|           (decl declaration?)) |#
#|   (decl-3 (identifyer symbol?) |#
#|           (exp exp?))) |#
#||#
#| (define-datatype exp exp? |#
#|   (exp-1 (var symbol?)) |#
#|   (exp-2 (a exp?) |#
#|          (b exp?))) |#
#||#
#| ;; Se imprimé en pantalla nuestro arbol de sintaxis abstracta |#
#| (display (decl-1 (decl-3 'x (exp-1 'x)) (decl-2 (exp-1 'x) (decl-3 'y (exp-2 (exp-1 'a) (exp-1 'b)))))) |#

;;Lo anterior lo podemos replicar con SLLGEN
(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)
    (comentario ("%" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number) ;Numero negativo
    (numero (digit (arbno digit) "." digit (arbno digit)) number) ;Numero coma flotante
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number) ;Numero coma flotante negativo
    ))

(define especificacion-gramatical
  '(
    (declaration ("{" declaration ";" declaration "}") decl-1)
    (declaration ("while" exp "do" declaration) decl-2)
    (declaration (identificador ":=" exp) decl-3)
    (exp (identificador) exp-1)
    (exp (numero) exp-2)
    (exp ( "(" exp  "+" exp ")") exp-3)
    )
  )
#|
<declaration> ::= {<declaration> ; <declaration>} 
              ::= while <expression> do <declaration>
              ::= identificador := expression

<expression> ::= <identificador>
             ::= (<expression> + <expression>)
|#

;;Creamos los datatypes automaticamente con SLLGEN
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)
;para listar los datatypes (observar que datatypes se presentan)
(define lista-datatypes (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical))

;contruir el parser
(define scanner
  ;La siguiente función retorna un #procedure
  ;cuando le pasamos una expresión al procedure, este devuelve una lista con las unidades léxicas signifcativas
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

;parser
(define parser
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;pruebas
(define exp-prueba
  "while hola do perro := (-4.4 + 1)")
(display (scanner exp-prueba))
(newline)
(display (parser exp-prueba))
(newline)

;Con la siguiiente función, "empezamos a desvaratar" los arboles
;de sintaxis absdtracta con los cases
(define evaluar-programa
  (lambda (prog)
    (cases declaration prog
      (decl-1 (d1 d2) (list d1 d2))
      (decl-2 (exp d1) (list exp d1))
      (decl-3 (id exp) (list id exp))
      )))

;Construir el prompt
(define interpretador
  (sllgen:make-rep-loop "--primer lenguaje: " evaluar-programa
                        (sllgen:make-stream-parser especificacion-lexica especificacion-gramatical)))

;prueba
(interpretador)
;se abrirá un prompt
;si al prompt le pasamos un expresión correcta para nuestra gramática y léxica entonces
;evaluar con { x := 1.8; while x do j := 1 } 

