#lang racket/base
(case (list 'y 'x)
  [((a b) (x y)) 'forwards]
  [((b a) (y x)) 'backwards])
