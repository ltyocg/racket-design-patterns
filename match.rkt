#lang racket
(define x '(1 2 3))
(define k-if #f)
(if (call/cc
     (lambda(k)
       (set! k-if k)
       (null? x)))
    '()
    (cdr x))
(k-if #f)