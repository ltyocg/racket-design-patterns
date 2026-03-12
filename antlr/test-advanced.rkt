#lang racket/base

(require "parser.rkt"
         "lexer.rkt"
         parser-tools/lex
         racket/list
         rackunit)

;; Test that RULE_REF and TOKEN_REF are properly distinguished
(displayln "\n=== Testing Token Differentiation ===")

(define test-tokens
  (lex-string "myRule MyToken ANOTHER_TOKEN anotherRule"))

(displayln "Tokens extracted:")
(for ([tok test-tokens])
  (define t (position-token-token tok))
  (displayln (format "  ~a: ~a"
                     (if (token? t) (token-name t) t)
                     (if (token? t) (token-value t) ""))))

;; Test parser rule spec with RULE_REF
(displayln "\n=== Testing Parser Rule with RULE_REF ===")
(define parser-rule-test
  "grammar Test;
   myRule: anotherRule | SOME_TOKEN ;
   ")

(define ast1 (parse-string parser-rule-test))
(displayln "✓ Parser rule with RULE_REF parsed successfully")
(displayln (format "  Grammar type: ~a" (grammar-spec-type ast1)))
(displayln (format "  Grammar name: ~a" (grammar-spec-name ast1)))

;; Test lexer rule spec with TOKEN_REF
(displayln "\n=== Testing Lexer Rule with TOKEN_REF ===")
(define lexer-rule-test
  "lexer grammar TestLexer;
   IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]* ;
   NUMBER: [0-9]+ ;
   WS: [ \\t\\r\\n]+ ;
   ")

(define ast2 (parse-string lexer-rule-test))
(displayln "✓ Lexer rule with TOKEN_REF parsed successfully")
(displayln (format "  Grammar type: ~a" (grammar-spec-type ast2)))
(displayln (format "  Number of rules: ~a" (length (grammar-spec-rules ast2))))

;; Test fragment lexer rule
(displayln "\n=== Testing Fragment Lexer Rule ===")
(define fragment-test
  "lexer grammar TestLexer;
   fragment DIGIT: [0-9] ;
   NUMBER: DIGIT+ ;
   ")

(define ast3 (parse-string fragment-test))
(define frag-rule (first (grammar-spec-rules ast3)))
(displayln "✓ Fragment rule parsed successfully")
(displayln (format "  Is fragment: ~a" (lexer-rule-spec-fragment? frag-rule)))
(displayln (format "  Rule name: ~a" (lexer-rule-spec-name frag-rule)))

;; Test parser rule with options
(displayln "\n=== Testing Parser Rule with Options ===")
(define options-test
  "grammar Test;
   options { tokenVocab=MyLexer; }
   myRule returns [int value]: IDENTIFIER ;
   ")

(define ast4 (parse-string options-test))
(displayln "✓ Grammar with options parsed successfully")
(displayln (format "  Has prequels: ~a" (not (null? (grammar-spec-prequels ast4)))))

;; Test character ranges
(displayln "\n=== Testing Character Ranges ===")
(define range-test
  "lexer grammar TestLexer;
   LETTER: 'a'..'z' | 'A'..'Z' ;
   ")

(define ast5 (parse-string range-test))
(displayln "✓ Character range parsed successfully")

;; Test wildcard
(displayln "\n=== Testing Wildcard ===")
(define wildcard-test
  "grammar Test;
   anyChar: . ;
   ")

(define ast6 (parse-string wildcard-test))
(displayln "✓ Wildcard parsed successfully")

;; Test EBNF suffixes
(displayln "\n=== Testing EBNF Suffixes ===")
(define ebnf-test
  "grammar Test;
   expr: term ('+' term)* ;
   optional: IDENTIFIER? ;
   oneOrMore: DIGIT+ ;
   ")

(define ast7 (parse-string ebnf-test))
(displayln "✓ EBNF suffixes parsed successfully")
(displayln (format "  Number of rules: ~a" (length (grammar-spec-rules ast7))))

(displayln "\n=== All Advanced Tests Passed! ===")
