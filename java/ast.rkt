#lang racket/base
(require racket/class)
(define Node
  (class object%))
(define CompilationUnit
  (class Node
    (init-field package-declaration imports types module)))
(provide all-defined-out)