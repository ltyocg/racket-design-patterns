#lang racket/base
(require (for-syntax racket/base)
         (for-syntax syntax/parse)
         parser-tools/yacc)
(define-syntax (ebnf-parser stx)
  (syntax-parse stx
    [(_ body ...)
     (printf (syntax->datum #'(body ...)))
     #'(parser body ...)]))
(provide ebnf-parser)
(module+ test
  (require racket/pretty)
  (pretty-display
   (syntax->datum
    (expand-once
     #'(ebnf-parser
        [start compilation-unit]
        [end EOF]
        [error (lambda (tok-ok? tok-name tok-value start end)
                 (error 'java-parser "Parse error at line ~a, col ~a: ~a ~a"
                        (position-line start) (position-col start)
                        tok-name tok-value))]
        [src-pos]
        [tokens empty-tokens tokens]
        [grammar
         [compilation-unit
          [() '()]]])))))