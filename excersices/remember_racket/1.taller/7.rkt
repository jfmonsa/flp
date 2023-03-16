(define up
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(list? (car lst))
       (cons 
         (up-aux (car lst))
         (cdr lst))])))

(define up-aux
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else 
        (cons (car lst)
              (up (cdr lst)))])))

(print
  (up '(1 2 (x y) z)))

;; (display (up '((x (y) z) p)))
