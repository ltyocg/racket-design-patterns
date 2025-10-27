#lang racket
(define docuemnt-interface
  (interface ()
    [put (-> string? any/c)]
    get
    children))