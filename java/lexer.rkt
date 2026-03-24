#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; ============================================================================
;; Java Lexer
;; Based on ANTLR Java grammar: https://github.com/antlr/grammars-v4/tree/master/java/java
;; Supports Java 17 features including records, sealed classes, pattern matching, etc.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Token Definitions
;; ----------------------------------------------------------------------------

(define-empty-tokens keywords
  (ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE
            DEFAULT DO DOUBLE ELSE ENUM EXTENDS FINAL FINALLY FLOAT FOR GOTO IF
            IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG NATIVE NEW PACKAGE PRIVATE
            PROTECTED PUBLIC RETURN SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
            THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE

            ; Java 8+ tokens
            ARROW COLONCOLON

            ; Java 9+ modules
            MODULE OPEN EXPORTS OPENS REQUIRES TRANSITIVE USES PROVIDES TO WITH

            ; Java 10+ type inference
            VAR

            ; Java 14+ switch expressions
            YIELD

            ; Java 14+ records
            RECORD

            ; Java 15+ sealed classes
            SEALED NON_SEALED PERMITS

            ; Java 17+ pattern matching
            WHEN

            ; Literals
            NULL_LITERAL

            ; Separators
            LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA DOT

            ; Operators
            ASSIGN GT LT BANG TILDE QUESTION COLON
            EQUAL LE GE NOTEQUAL AND OR
            INC DEC ADD SUB MUL DIV BITAND BITOR CARET MOD
            ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN
            AND_ASSIGN OR_ASSIGN XOR_ASSIGN MOD_ASSIGN
            LSHIFT_ASSIGN RSHIFT_ASSIGN URSHIFT_ASSIGN

            ; Additional symbols
            AT ELLIPSIS

            ; Shift operators
            LSHIFT RSHIFT URSHIFT

            ; End of file
            EOF))

(define-tokens literals
  (IDENTIFIER        ; 标识符
   DECIMAL_LITERAL   ; 十进制整数
   HEX_LITERAL       ; 十六进制整数
   OCT_LITERAL       ; 八进制整数
   BINARY_LITERAL    ; 二进制整数
   FLOAT_LITERAL     ; 浮点数
   HEX_FLOAT_LITERAL ; 十六进制浮点数
   CHAR_LITERAL      ; 字符字面量
   STRING_LITERAL    ; 字符串字面量
   TEXT_BLOCK        ; 文本块 (Java 15+)
   BOOL_LITERAL      ; 布尔字面量 (带值)
   ))

;; ----------------------------------------------------------------------------
;; Lexer
;; ----------------------------------------------------------------------------

(define java-lexer
  (lexer-src-pos
   ;; Whitespace (skip)
   [whitespace
    (return-without-pos (java-lexer input-port))]

   ;; Traditional comment /* ... */
   ["/*"
    (begin
      (let loop ()
        (let ([c (read-char input-port)])
          (cond
            [(eof-object? c) (error "Unterminated comment")]
            [(and (char=? c #\*)
                  (let ([next (peek-char input-port)])
                    (and (char? next) (char=? next #\/))))
             (read-char input-port)] ; consume the /
            [else (loop)])))
      (return-without-pos (java-lexer input-port)))]

   ;; End-of-line comment // ...
   [(:: "//" (:* (char-complement #\newline)))
    (return-without-pos (java-lexer input-port))]

   ;; Text Block (Java 15+): """ ... """
   [(:: "\"\"\"" (:* (char-complement #\")) "\"\"\"")
    (token-TEXT_BLOCK lexeme)]

   ;; String literal
   [(:: #\" (:* (:or (:: #\\ any-char) (char-complement #\"))) #\")
    (token-STRING_LITERAL (substring lexeme 1 (- (string-length lexeme) 1)))]

   ;; Character literal
   [(:: #\' (:or (:: #\\ any-char) (char-complement #\')) #\')
    (token-CHAR_LITERAL (string-ref lexeme 1))]

   ;; Hex float literal: 0x...p... (supports underscores and hex digits)
   [(:: (:or "0x" "0X")
        (:+ (:or numeric #\a #\b #\c #\d #\e #\f
                 #\A #\B #\C #\D #\E #\F #\_))
        (:or "." "")
        (:* (:or numeric #\a #\b #\c #\d #\e #\f
                 #\A #\B #\C #\D #\E #\F #\_))
        (:or "p" "P")
        (:? (:or "+" "-"))
        (:+ numeric)
        (:? (:or "f" "F" "d" "D")))
    (token-HEX_FLOAT_LITERAL lexeme)]

   ;; Float literal with decimal point (supports underscores)
   [(:: (:+ (:or numeric #\_))
        #\.
        (:* (:or numeric #\_))
        (:? (:: (:or "e" "E") (:? (:or "+" "-")) (:+ (:or numeric #\_))))
        (:? (:or "f" "F" "d" "D")))
    (token-FLOAT_LITERAL lexeme)]

   ;; Float literal starting with decimal point (supports underscores)
   [(:: #\.
        (:+ (:or numeric #\_))
        (:? (:: (:or "e" "E") (:? (:or "+" "-")) (:+ (:or numeric #\_))))
        (:? (:or "f" "F" "d" "D")))
    (token-FLOAT_LITERAL lexeme)]

   ;; Float literal with exponent only (supports underscores)
   [(:: (:+ (:or numeric #\_))
        (:or "e" "E")
        (:? (:or "+" "-"))
        (:+ (:or numeric #\_))
        (:? (:or "f" "F" "d" "D")))
    (token-FLOAT_LITERAL lexeme)]

   ;; Float literal with suffix only (supports underscores)
   [(:: (:+ (:or numeric #\_))
        (:or "f" "F" "d" "D"))
    (token-FLOAT_LITERAL lexeme)]

   ;; Binary literal: 0b...
   [(:: (:or "0b" "0B")
        (:+ (:or #\0 #\1 #\_))
        (:? (:or "l" "L")))
    (token-BINARY_LITERAL lexeme)]

   ;; Hex literal: 0x...
   [(:: (:or "0x" "0X")
        (:+ (:or numeric #\a #\b #\c #\d #\e #\f
                 #\A #\B #\C #\D #\E #\F #\_))
        (:? (:or "l" "L")))
    (token-HEX_LITERAL lexeme)]

   ;; Octal literal: 0...
   [(:: #\0
        (:+ (:or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\_))
        (:? (:or "l" "L")))
    (token-OCT_LITERAL lexeme)]

   ;; Decimal literal
   [(:: (:or #\0 (:: (char-range #\1 #\9) (:* (:or numeric #\_))))
        (:? (:or "l" "L")))
    (token-DECIMAL_LITERAL lexeme)]

   ;; Ellipsis ...
   ["..." (token-ELLIPSIS)]

   ;; Double colon ::
   ["::" (token-COLONCOLON)]

   ;; Arrow ->
   ["->" (token-ARROW)]

   ;; Compound assignment operators (must come before simple operators)
   [">>>=" (token-URSHIFT_ASSIGN)]
   [">>=" (token-RSHIFT_ASSIGN)]
   ["<<=" (token-LSHIFT_ASSIGN)]
   ["+=" (token-ADD_ASSIGN)]
   ["-=" (token-SUB_ASSIGN)]
   ["*=" (token-MUL_ASSIGN)]
   ["/=" (token-DIV_ASSIGN)]
   ["%=" (token-MOD_ASSIGN)]
   ["&=" (token-AND_ASSIGN)]
   ["|=" (token-OR_ASSIGN)]
   ["^=" (token-XOR_ASSIGN)]

   ;; Shift operators (must come before single < >)
   [">>>" (token-URSHIFT)]
   [">>" (token-RSHIFT)]
   ["<<" (token-LSHIFT)]

   ;; Comparison operators
   ["==" (token-EQUAL)]
   ["!=" (token-NOTEQUAL)]
   ["<=" (token-LE)]
   [">=" (token-GE)]
   ["&&" (token-AND)]
   ["||" (token-OR)]

   ;; Increment/decrement
   ["++" (token-INC)]
   ["--" (token-DEC)]

   ;; Simple operators
   ["=" (token-ASSIGN)]
   [">" (token-GT)]
   ["<" (token-LT)]
   ["!" (token-BANG)]
   ["~" (token-TILDE)]
   ["?" (token-QUESTION)]
   [":" (token-COLON)]
   ["+" (token-ADD)]
   ["-" (token-SUB)]
   ["*" (token-MUL)]
   ["/" (token-DIV)]
   ["%" (token-MOD)]
   ["&" (token-BITAND)]
   ["|" (token-BITOR)]
   ["^" (token-CARET)]

   ;; Separators
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["[" (token-LBRACK)]
   ["]" (token-RBRACK)]
   [";" (token-SEMI)]
   ["," (token-COMMA)]
   ["." (token-DOT)]
   ["@" (token-AT)]

   ;; Keywords - Java 17+
   ["abstract" (token-ABSTRACT)]
   ["assert" (token-ASSERT)]
   ["boolean" (token-BOOLEAN)]
   ["break" (token-BREAK)]
   ["byte" (token-BYTE)]
   ["case" (token-CASE)]
   ["catch" (token-CATCH)]
   ["char" (token-CHAR)]
   ["class" (token-CLASS)]
   ["const" (token-CONST)]
   ["continue" (token-CONTINUE)]
   ["default" (token-DEFAULT)]
   ["do" (token-DO)]
   ["double" (token-DOUBLE)]
   ["else" (token-ELSE)]
   ["enum" (token-ENUM)]
   ["exports" (token-EXPORTS)]
   ["extends" (token-EXTENDS)]
   ["final" (token-FINAL)]
   ["finally" (token-FINALLY)]
   ["float" (token-FLOAT)]
   ["for" (token-FOR)]
   ["goto" (token-GOTO)]
   ["if" (token-IF)]
   ["implements" (token-IMPLEMENTS)]
   ["import" (token-IMPORT)]
   ["instanceof" (token-INSTANCEOF)]
   ["int" (token-INT)]
   ["interface" (token-INTERFACE)]
   ["long" (token-LONG)]
   ["module" (token-MODULE)]
   ["native" (token-NATIVE)]
   ["new" (token-NEW)]
   ["non-sealed" (token-NON_SEALED)]
   ["open" (token-OPEN)]
   ["opens" (token-OPENS)]
   ["package" (token-PACKAGE)]
   ["permits" (token-PERMITS)]
   ["private" (token-PRIVATE)]
   ["protected" (token-PROTECTED)]
   ["provides" (token-PROVIDES)]
   ["public" (token-PUBLIC)]
   ["record" (token-RECORD)]
   ["requires" (token-REQUIRES)]
   ["return" (token-RETURN)]
   ["sealed" (token-SEALED)]
   ["short" (token-SHORT)]
   ["static" (token-STATIC)]
   ["strictfp" (token-STRICTFP)]
   ["super" (token-SUPER)]
   ["switch" (token-SWITCH)]
   ["synchronized" (token-SYNCHRONIZED)]
   ["this" (token-THIS)]
   ["throw" (token-THROW)]
   ["throws" (token-THROWS)]
   ["to" (token-TO)]
   ["transient" (token-TRANSIENT)]
   ["transitive" (token-TRANSITIVE)]
   ["try" (token-TRY)]
   ["uses" (token-USES)]
   ["var" (token-VAR)]
   ["void" (token-VOID)]
   ["volatile" (token-VOLATILE)]
   ["when" (token-WHEN)]
   ["while" (token-WHILE)]
   ["with" (token-WITH)]
   ["yield" (token-YIELD)]

   ;; Boolean and null literals
   ["true" (token-BOOL_LITERAL #t)]
   ["false" (token-BOOL_LITERAL #f)]
   ["null" (token-NULL_LITERAL)]

   ;; Identifier: Letter (Letter | Digit)*
   [(:: (:or alphabetic #\_ #\$)
        (:* (:or alphabetic numeric #\_ #\$)))
    (token-IDENTIFIER lexeme)]

   ;; End of file
   [(eof) (token-EOF)]))

;; ----------------------------------------------------------------------------
;; Exports
;; ----------------------------------------------------------------------------

(provide (all-defined-out))
