#lang racket
(module stxclass-mod racket
  (require syntax/parse)
  (define-syntax-class foo
    (pattern (a b c)))
  (provide foo))
(module macro-mod racket
  (require (for-syntax syntax/parse
                       (submod ".." stxclass-mod)))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)]))
  (provide macro))
(require 'macro-mod)
(pretty-display (syntax->datum (expand-syntax-once #'(macro (1 2 3)))))