#lang racket/base

(require "parser.rkt"
         "lexer.rkt"
         parser-tools/lex)

;; Test that RULE_REF and TOKEN_REF are properly distinguished
(displayln "=== Testing Token Differentiation ===\n")

(define test-string "myRule MyToken ANOTHER_TOKEN anotherRule")
(define port (open-input-string test-string))

(displayln (format "Input: ~a\n" test-string))
(displayln "Tokens:")

(let loop ()
  (define tok (antlr4-lexer port))
  (define t (position-token-token tok))
  (unless (eq? t 'EOF)
    (displayln (format "  ~a: ~a"
                       (if (token? t) (token-name t) t)
                       (if (token? t) (token-value t) "")))
    (loop)))

;; Test parsing
(displayln "\n=== Testing Parsing ===\n")

(define test-grammars
  (list
   (cons "Basic grammar"
         "grammar Test;
          expr: term '+' expr | term ;
          term: IDENTIFIER | NUMBER ;
          IDENTIFIER: [a-z]+ ;
          NUMBER: [0-9]+ ;
          ")
   (cons "Lexer grammar"
         "lexer grammar TestLexer;
          IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]* ;
          NUMBER: [0-9]+ ;
          ")
   (cons "Parser grammar with options"
         "grammar Test;
          options { tokenVocab=MyLexer; }
          expr: IDENTIFIER ;
          ")
   (cons "Fragment rule"
         "lexer grammar TestLexer;
          fragment DIGIT: [0-9] ;
          NUMBER: DIGIT+ ;
          ")))

(for ([test test-grammars])
  (define name (car test))
  (define grammar (cdr test))
  (displayln (format "Testing: ~a" name))
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (format "  ✗ FAILED: ~a" (exn-message e))))])
    (define ast (parse-string grammar))
    (displayln (format "  ✓ Parsed successfully (type: ~a, name: ~a)"
                       (grammar-spec-type ast)
                       (grammar-spec-name ast)))))

(displayln "\n=== Done ===")
