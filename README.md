#Lambda Calculus

Lambda Calculus are the basics for functional programming languages such as haskell or lisp, also Lambda calculus is present in most of the mainstream programming languages

In the other paradigms functions are treated as first class citizens, on the other hand, in lambda calculus functions are the only kind of citizen, all values are functions, functions are the only type of thing.

Functions more or less only do one thing, they take values, consume them and plug them inside the function, and of course this plugged values are themselves functions because that's the only thing the can be, they also consume values and plug them, and so on until there's nothing left to consume

example:
"""scheme
(define multiplique
(lambda (a b) (* a (* b b)))
)
"""

## Lambda Calculus and functional programming paradigm
In functional programming paradigm there's no difference between functions and values, it means that we can pass anonymous functions (lambda functions) directly as parameter to other function
"""scheme
(define (lambda x (lambda y)(+ x y ))
"""

## Sources

- [lambda calculus - Computerphile](https://www.youtube.com/watch?v=eis11j_iGMs)