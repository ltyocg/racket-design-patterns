#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

;; ======================================================
;; ANTLR v4 Lexer - ported from ANTLRv4Lexer.g4
;; ======================================================

;; Token types
(define-tokens tokens
  (;; Comments
   DOC_COMMENT
   BLOCK_COMMENT
   LINE_COMMENT
   ;; Integer
   INT
   ;; String literals
   STRING_LITERAL
   UNTERMINATED_STRING_LITERAL
   ;; Arguments
   BEGIN_ARGUMENT
   ACTION
   ARGUMENT_CONTENT
   END_ARGUMENT
   UNTERMINATED_ARGUMENT
   ;; Keywords
   OPTIONS
   TOKENS
   CHANNELS
   IMPORT
   FRAGMENT
   LEXER
   PARSER
   GRAMMAR
   PROTECTED
   PUBLIC
   PRIVATE
   RETURNS
   LOCALS
   THROWS
   CATCH
   FINALLY
   MODE
   ;; Punctuation
   COLON
   COLONCOLON
   COMMA
   SEMI
   LPAREN
   RPAREN
   RBRACE
   RARROW
   LT
   GT
   ASSIGN
   QUESTION
   STAR
   PLUS_ASSIGN
   PLUS
   OR
   DOLLAR
   RANGE
   DOT
   AT
   POUND
   NOT
   ;; Identifiers
   RULE_REF
   TOKEN_REF
   ID
   ;; Whitespace
   WS
   ;; Lexer char set
   LEXER_CHAR_SET
   UNTERMINATED_CHAR_SET
   ;; EOF
   EOF))

;; ======================================================
;; Fragments (helper patterns)
;; ======================================================

;; Hex digit
(define-lex-abbrev hex-digit
  (:or (:/ #\0 #\9) (:/ #\a #\f) (:/ #\A #\F)))

;; Unicode escape sequence
(define-lex-abbrev unicode-esc
  (:seq "u"
        (:? hex-digit
            (:? hex-digit
                (:? hex-digit)))))

;; Escape sequence
(define-lex-abbrev esc-sequence
  (:seq "\\"
        (:or "b" "t" "n" "f" "r" "\"" "'" "\\"
             unicode-esc
             (:/ #\000 #\177)
             any-char)))

;; Name start character (simplified for common use)
(define-lex-abbrev name-start-char
  (:or (:/ #\A #\Z)
       (:/ #\a #\z)))

;; Name character
(define-lex-abbrev name-char
  (:or name-start-char
       (:/ #\0 #\9)
       "_"))

;; Identifier
(define-lex-abbrev identifier
  (:seq name-start-char (:* name-char)))

;; Whitespace
(define-lex-abbrev ws-char
  (:or #\space #\tab #\newline #\return #\page))

;; ======================================================
;; Main Lexer
;; ======================================================

(define antlr4-lexer
  (lexer-src-pos
   ;; -------------------------
   ;; Comments (skip them)
   ;;
   ;; DOC_COMMENT: /** ... */
   [(:or (:seq "/**" (:* (:~ #\*)) "*/")
         (:seq "/**" (:* (:or (:~ #\*) (:seq "*" (:~ #\/)))) "*")
         (:seq "/**" (:* (:~ #\*)) (:seq "*" (:~ #\/)) (:* (:~)) "*/"))
    (return-without-pos (antlr4-lexer input-port))]

   ;; BLOCK_COMMENT: /* ... */
   [(:seq "/*"
          (:* (:or (:~ #\*) (:seq "*" (:~ #\/))))
          (:? "*/"))
    (return-without-pos (antlr4-lexer input-port))]

   ;; LINE_COMMENT: // ...
   [(:seq "//" (:* (:~ #\newline #\return)))
    (return-without-pos (antlr4-lexer input-port))]

   ;; -------------------------
   ;; Keywords (must come before ID)
   ;;
   ;; OPTIONS: options {
   [(:seq "options" (:* ws-char) "{")
    (token-OPTIONS lexeme)]

   ;; TOKENS: tokens {
   [(:seq "tokens" (:* ws-char) "{")
    (token-TOKENS lexeme)]

   ;; CHANNELS: channels {
   [(:seq "channels" (:* ws-char) "{")
    (token-CHANNELS lexeme)]

   ;; Other keywords
   ["import" (token-IMPORT lexeme)]
   ["fragment" (token-FRAGMENT lexeme)]
   ["lexer" (token-LEXER lexeme)]
   ["parser" (token-PARSER lexeme)]
   ["grammar" (token-GRAMMAR lexeme)]
   ["protected" (token-PROTECTED lexeme)]
   ["public" (token-PUBLIC lexeme)]
   ["private" (token-PRIVATE lexeme)]
   ["returns" (token-RETURNS lexeme)]
   ["locals" (token-LOCALS lexeme)]
   ["throws" (token-THROWS lexeme)]
   ["catch" (token-CATCH lexeme)]
   ["finally" (token-FINALLY lexeme)]
   ["mode" (token-MODE lexeme)]

   ;; -------------------------
   ;; Integer
   ;;
   ;; INT: 0 | [1-9][0-9]*
   ["0" (token-INT lexeme)]
   [(:seq (:/ #\1 #\9) (:* (:/ #\0 #\9)))
    (token-INT lexeme)]

   ;; -------------------------
   ;; String literals
   ;;
   ;; STRING_LITERAL: '...' with escape sequences
   [(:seq "'"
          (:* (:or esc-sequence
                  (:~ #\' #\newline #\return #\\)))
          "'")
    (token-STRING_LITERAL lexeme)]

   ;; UNTERMINATED_STRING_LITERAL
   [(:seq "'"
          (:* (:or esc-sequence
                  (:~ #\' #\newline #\return #\\))))
    (token-UNTERMINATED_STRING_LITERAL lexeme)]

   ;; -------------------------
   ;; Punctuation (longer operators first)
   ;;
   ["::" (token-COLONCOLON lexeme)]
   [":" (token-COLON lexeme)]
   ["," (token-COMMA lexeme)]
   [";" (token-SEMI lexeme)]
   ["(" (token-LPAREN lexeme)]
   [")" (token-RPAREN lexeme)]
   ["}" (token-RBRACE lexeme)]
   ["->" (token-RARROW lexeme)]
   ["<" (token-LT lexeme)]
   [">" (token-GT lexeme)]
   ["=" (token-ASSIGN lexeme)]
   ["?" (token-QUESTION lexeme)]
   ["*" (token-STAR lexeme)]
   ["+=" (token-PLUS_ASSIGN lexeme)]
   ["+" (token-PLUS lexeme)]
   ["|" (token-OR lexeme)]
   ["$" (token-DOLLAR lexeme)]
   [".." (token-RANGE lexeme)]
   ["." (token-DOT lexeme)]
   ["@" (token-AT lexeme)]
   ["#" (token-POUND lexeme)]
   ["~" (token-NOT lexeme)]
   ["-" (token-RANGE lexeme)]  ;; 单独的连字符

   ;; -------------------------
   ;; LEXER_CHAR_SET: [...] (must come before BEGIN_ARGUMENT)
   ;; Matches character class like [a-z], [A-Za-z0-9], etc.
   [(:seq "["
          (:* (:or (:seq "\\" any-char)
                   (:seq (:~ #\] #\\ #\newline #\return) (:? "-") (:~ #\] #\\ #\newline #\return))
                   (:~ #\] #\\ #\newline #\return)))
          "]")
    (token-LEXER_CHAR_SET lexeme)]

   ;; -------------------------
   ;; BEGIN_ARGUMENT: [ (only if not a char set)
   ;; This is for parser rule arguments like rule[int x, int y]
   ;; In practice, this is handled differently - see argument mode below
   ;; ["[" (token-BEGIN_ARGUMENT lexeme)]
   ;; ["]" (token-END_ARGUMENT lexeme)]

   ;; Backslash for escape sequences
   ["\\" (token-ID lexeme)]

   ;; -------------------------
   ;; ACTION: { ... } (nested braces)
   ;; Simplified version that handles basic nested braces
   [(:seq "{"
          (:* (:or "{" "}"
                   esc-sequence
                   (:seq "'" (:* (:or esc-sequence (:~ #\' #\\))) "'")
                   (:seq "\"" (:* (:or esc-sequence (:~ #\" #\\))) "\"")
                   (:~ #\{ #\} #\" #\' #\\)))
          "}")
    (token-ACTION lexeme)]

   ;; -------------------------
   ;; Identifiers
   ;; TOKEN_REF: uppercase start (A-Z)
   [(:seq (:/ #\A #\Z) (:* name-char))
    (token-TOKEN_REF lexeme)]

   ;; RULE_REF: lowercase start (a-z)
   [(:seq (:/ #\a #\z) (:* name-char))
    (token-RULE_REF lexeme)]

   ;; Generic identifier fallback
   [identifier (token-ID lexeme)]

   ;; -------------------------
   ;; Whitespace (skip it)
   [(:+ ws-char)
    (return-without-pos (antlr4-lexer input-port))]

   ;; -------------------------
   ;; EOF
   [(eof) (token-EOF lexeme)]

   ;; Any other character (fallback)
   [any-char (list 'UNKNOWN lexeme)]))

;; ======================================================
;; Lexer modes
;; ======================================================

;; Argument mode lexer
(define argument-mode-lexer
  (lexer-src-pos
   ;; Nested argument
   ["[" (token-ARGUMENT_CONTENT lexeme)]

   ;; End argument
   ["]" (token-END_ARGUMENT lexeme)]

   ;; String literals in arguments
   [(:seq "'" (:* (:or esc-sequence (:~ #\' #\newline #\return #\\))) "'")
    (token-ARGUMENT_CONTENT lexeme)]

   [(:seq "\"" (:* (:or esc-sequence (:~ #\" #\newline #\return #\\))) "\"")
    (token-ARGUMENT_CONTENT lexeme)]

   ;; Escape
   [(:seq "\\" (:? (:~ #\newline #\return)))
    (token-ARGUMENT_CONTENT lexeme)]

   ;; Any other content
   [(:+ (:~ #\[ #\] #\' #\" #\\ #\newline #\return))
    (token-ARGUMENT_CONTENT lexeme)]

   ;; Single character fallback
   [(special) (token-ARGUMENT_CONTENT lexeme)]

   ;; EOF
   [(eof) (token-UNTERMINATED_ARGUMENT lexeme)]))

;; Lexer char set mode
(define lexer-char-set-mode-lexer
  (lexer-src-pos
   ;; Char set content (continue accumulating)
   [(:+ (:or (:~ #\] #\\) (:seq "\\" (:? (:~ #\newline #\return)))))
    (return-without-pos (lexer-char-set-mode-lexer input-port))]

   ;; End of char set
   ["]" (token-LEXER_CHAR_SET lexeme)]

   ;; EOF
   [(eof) (token-UNTERMINATED_CHAR_SET lexeme)]))

;; ======================================================
;; Lexer state management
;; ======================================================

;; Lexer state: tracks current mode
(struct lexer-state (mode bracket-depth) #:mutable)

(define (make-lexer-state)
  (lexer-state 'default 0))

;; ======================================================
;; Tokenize function
;; ======================================================

(define (tokenize port [state (make-lexer-state)])
  (define (next-token)
    (case (lexer-state-mode state)
      [(default) (antlr4-lexer port)]
      [(argument) (argument-mode-lexer port)]
      [(char-set) (lexer-char-set-mode-lexer port)]
      [else (antlr4-lexer port)]))
  (next-token))

;; ======================================================
;; Convenience function to lex a string
;; ======================================================

(define (lex-string str)
  (let ([port (open-input-string str)])
    (let loop ([tokens '()])
      (let ([tok (antlr4-lexer port)])
        (if (eq? (position-token-token tok) 'EOF)
            (reverse (cons tok tokens))
            (loop (cons tok tokens)))))))

;; ======================================================
;; Test examples
;; ======================================================

(module+ test
  (require rackunit)

  ;; Helper to get token type from position-token
  (define (token-type tok)
    (let ([t (position-token-token tok)])
      (if (token? t) (token-name t) t)))

  ;; Test integer
  (check-equal? (token-type (antlr4-lexer (open-input-string "0"))) 'INT)
  (check-equal? (token-type (antlr4-lexer (open-input-string "123"))) 'INT)

  ;; Test keywords
  (check-equal? (token-type (antlr4-lexer (open-input-string "grammar"))) 'GRAMMAR)
  (check-equal? (token-type (antlr4-lexer (open-input-string "lexer"))) 'LEXER)

  ;; Test punctuation
  (check-equal? (token-type (antlr4-lexer (open-input-string ":"))) 'COLON)
  (check-equal? (token-type (antlr4-lexer (open-input-string "->"))) 'RARROW)

  ;; Test identifier
  (check-equal? (token-type (antlr4-lexer (open-input-string "myRule"))) 'ID)

  ;; Test string literal
  (check-equal? (token-type (antlr4-lexer (open-input-string "'hello'"))) 'STRING_LITERAL)

  ;; Test comment
  (check-equal? (token-type (antlr4-lexer (open-input-string "// comment"))) 'LINE_COMMENT)

  ;; Test whitespace
  (check-equal? (token-type (antlr4-lexer (open-input-string "   "))) 'WS))
