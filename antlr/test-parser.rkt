#lang racket/base

(require "parser.rkt"
         "lexer.rkt")

;; Test simple grammar
(define test-grammar-1
  "grammar Test;

   expr: term '+' expr
       | term
       ;

   term: IDENTIFIER
       | NUMBER
       ;

   IDENTIFIER: [a-z]+ ;
   NUMBER: [0-9]+ ;
   ")

;; Test lexer grammar
(define test-grammar-2
  "lexer grammar TestLexer;

   IDENTIFIER: [a-z]+ ;
   NUMBER: [0-9]+ ;
   WS: [ \\t\\r\\n]+ -> skip ;
   ")

;; Test parser grammar
(define test-grammar-3
  "parser grammar TestParser;

   options { tokenVocab=TestLexer; }

   expr: term '+' expr
       | term
       ;

   term: identifier
       | number
       ;

   identifier: IDENTIFIER ;
   number: NUMBER ;
   ")

;; Run tests
(module+ test
  (require rackunit)

  (displayln "Testing basic grammar...")
  (check-not-exn
   (lambda ()
     (parse-string test-grammar-1)))

  (displayln "Testing lexer grammar...")
  (check-not-exn
   (lambda ()
     (parse-string test-grammar-2)))

  (displayln "Testing parser grammar...")
  (check-not-exn
   (lambda ()
     (parse-string test-grammar-3)))

  (displayln "All tests passed!"))

;; Run when module is executed
(module+ main
  (require (submod ".." test)))
