#lang racket
(require syntax/parse
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "lexer.rkt")
(define-syntax (ebnf-parser stx)
  (syntax-parse stx
    []))
(define java-parser
  (parser
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
     [() '()]]]))