#lang racket/base
(require racket/class)
(define document<%>
  (interface ()
    open close read-byte write-byte))