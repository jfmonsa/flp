(define arbol1
  '(2 (1 2 3) (4 5 ())))

(define common-subtree
  (lambda (arb1 arb2)
    (cond
      [(or
         (null? arb1)
         (null? arb2))
       '()]
      [(and
         (number? (car arb1))
         (number? (car arb2)))
       (list 0)]
      [else
        (list
          0
          (common-subtree (cadr arb1) (cadr arb2))
          (common-subtree (caddr arb1) (caddr arb2)))])))

(display
  (common-subtree
    '(0 (0
         (0 (0 () ())
             (0 () ()))
         (0 () ()))))
    '(0
      (0
       (0
        (0 () ())
        ())
       ))
  )

