#| (define list-facts
  (lambda (n)
    (cond
      [(<= n 0) '()]
      [else
        (letrec
          (
           (list-facts-aux
             (lambda (acc n)
               (cond
                 [(= acc n)(list (factorial acc))]
                 [else
                   (cons
                     (factorial acc)
                     (list-facts-aux (+ acc 1) n))])))
           ))]))) |#
(define list-facts
  (lambda (n [acc 1])
    (cond
      [(or 
         (<= n 0)
         (> acc n)
         ) '()]
      [else
        (cond
          (factorial acc)
          (list-facts n (+ acc 1)))])))

(define factorial
  (lambda (n)
    (cond
      [(= n 1) 1]
      [else ()])))
;paso de parametros por defecto
