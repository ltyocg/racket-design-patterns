#lang s-exp syntax/module-reader
(define-grammar-operator (* s)
  [_
   [() '()]
   [(s _) (cons $1 $2)]])