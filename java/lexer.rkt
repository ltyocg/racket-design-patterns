#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/runtime-path)
(define-empty-tokens empty-tokens
  (ABSTRACT
   ASSERT
   BOOLEAN
   BREAK
   BYTE
   CASE
   CATCH
   CHAR
   CLASS
   CONST
   CONTINUE
   DEFAULT
   DO
   DOUBLE
   ELSE
   ENUM
   EXPORTS
   EXTENDS
   FINAL
   FINALLY
   FLOAT
   FOR
   GOTO
   IF
   IMPLEMENTS
   IMPORT
   INSTANCEOF
   INT
   INTERFACE
   LONG
   MODULE
   NATIVE
   NEW
   NON_SEALED
   OPEN
   OPENS
   PACKAGE
   PERMITS
   PRIVATE
   PROTECTED
   PROVIDES
   PUBLIC
   RECORD
   REQUIRES
   RETURN
   SEALED
   SHORT
   STATIC
   STRICTFP
   SUPER
   SWITCH
   SYNCHRONIZED
   THIS
   THROW
   THROWS
   TO
   TRANSIENT
   TRANSITIVE
   TRY
   USES
   VAR
   VOID
   VOLATILE
   WHEN
   WHILE
   WITH
   YIELD
   NULL_LITERAL
   LPAREN
   RPAREN
   LBRACE
   RBRACE
   LBRACK
   RBRACK
   SEMI
   COMMA
   DOT
   ASSIGN
   GT
   LT
   BANG
   TILDE
   QUESTION
   COLON
   EQUAL
   LE
   GE
   NOTEQUAL
   AND
   OR
   INC
   DEC
   ADD
   SUB
   MUL
   DIV
   BITAND
   BITOR
   CARET
   MOD
   ADD_ASSIGN
   SUB_ASSIGN
   MUL_ASSIGN
   DIV_ASSIGN
   AND_ASSIGN
   OR_ASSIGN
   XOR_ASSIGN
   MOD_ASSIGN
   LSHIFT_ASSIGN
   RSHIFT_ASSIGN
   URSHIFT_ASSIGN
   ARROW
   COLONCOLON
   AT
   ELLIPSIS
   EOF))
(define-tokens tokens
  (DECIMAL_LITERAL
   HEX_LITERAL
   OCT_LITERAL
   BINARY_LITERAL
   FLOAT_LITERAL
   HEX_FLOAT_LITERAL
   BOOL_LITERAL
   CHAR_LITERAL
   STRING_LITERAL
   TEXT_BLOCK
   WS
   COMMENT
   LINE_COMMENT
   IDENTIFIER))
(define-lex-abbrev ExponentPart
  (:: (char-set "eE")
      (:? (char-set "+-"))
      (:+ Digits)))
(define-lex-abbrev EscapeSequence
  (:or (:: #\\
           (:? "u005c")
           (char-set "bstnfr\"'\\"))
       (:: #\\
           (:? "u005c")
           (:? (:: (:? (:/ #\0 #\3))
                   (:/ #\0 #\7)))
           (:/ #\0 #\7))
       (:: #\\
           (:+ #\u)
           HexDigit
           HexDigit
           HexDigit
           HexDigit)))
(define-lex-abbrev HexDigits
  (:: HexDigit
      (:? (:: (:* (:or HexDigit #\_))
              HexDigit))))
(define-lex-abbrev HexDigit
  (:or (:/ #\0 #\9)
       (:/ #\a #\f)
       (:/ #\A #\F)))
(define-lex-abbrev Digits
  (:: (:/ #\0 #\9)
      (:? (:: (:or (:/ #\0 #\9) #\_)
              (:/ #\0 #\9)))))
(define-lex-abbrev LetterOrDigit
  (:or Letter
       (:/ #\0 #\9)))
(define-lex-abbrev Letter
  (:or (:/ #\a #\z)
       (:/ #\A #\Z)
       (char-set "$_")
       (char-complement (:/ #\u0000 #\u007F))))

(define java-lexer
  (lexer-src-pos
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
   [(:: (:or #\0
             (:: (:/ #\1 #\9)
                 (:or (:? Digits)
                      (:: (:+ #\_) Digits))))
        (:? (char-set "lL")))
    (token-DECIMAL_LITERAL (string->number lexeme))]
   [(:: #\0
        (char-set "xX")
        (:or (:/ #\0 #\9)
             (:/ #\a #\f)
             (:/ #\A #\F))
        (:? (:: (:* (:or (:/ #\0 #\9)
                         (:/ #\a #\f)
                         (:/ #\A #\F)
                         #\_))
                (:or (:/ #\0 #\9)
                     (:/ #\a #\f)
                     (:/ #\A #\F))))
        (:? (char-set "lL")))
    (token-HEX_LITERAL lexeme)]
   [(:: #\0
        (:* #\_)
        (:/ #\0 #\7)
        (:? (:: (:* (:or (:/ #\0 #\7)
                         #\_))
                (:or (:/ #\0 #\7))))
        (:? (char-set "lL"))) 
    (token-OCT_LITERAL lexeme)]
   [(:: #\0
        (char-set "bB")
        (char-set "01")
        (:? (:: (:* (:or (char-set "01")
                         #\_))
                (:or (char-set "01"))))
        (:? (char-set "lL")))
    (token-BINARY_LITERAL lexeme)]
   [(:or (:: (:or (:: Digits #\. (:? Digits))
                  (:: #\. Digits))
             (:? ExponentPart)
             (:? (char-set "fFdD")))
         (:: Digits
             (:or (:: ExponentPart (:? (char-set "fFdD")))
                  (char-set "fFdD"))))
    (token-FLOAT_LITERAL lexeme)]
   [(:: #\0
        (char-set "xX")
        (:or (:: HexDigits (:? #\.))
             (:: (:? HexDigits) #\. HexDigits))
        (char-set "pP")
        (:? (char-set "+-"))
        Digits
        (:? (char-set "fFdD")))
    (token-HEX_FLOAT_LITERAL lexeme)]
   ["true" (token-BOOL_LITERAL #t)]
   ["false" (token-BOOL_LITERAL #f)]
   [(:: #\'
        (:or (char-complement (char-set "'\\\r\n"))
             EscapeSequence)
        #\')
    (token-CHAR_LITERAL (string-ref lexeme 1))]
   [(:: #\"
        (:* (:or (char-complement (char-set "\"\\\r\n"))
                 EscapeSequence))
        #\")
    (token-STRING_LITERAL (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:: "\"\"\""
        (:* (char-set " \t"))
        (char-set "\r\n")
        (:? (:* (:or any-char EscapeSequence)))
        "\"\"\"")
    (token-TEXT_BLOCK lexeme)]
   ["null" (token-NULL_LITERAL)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["[" (token-LBRACK)]
   ["]" (token-RBRACK)]
   [";" (token-SEMI)]
   ["," (token-COMMA)]
   ["." (token-DOT)]
   ["=" (token-ASSIGN)]
   [">" (token-GT)]
   ["<" (token-LT)]
   ["!" (token-BANG)]
   ["~" (token-TILDE)]
   ["?" (token-QUESTION)]
   [":" (token-COLON)]
   ["==" (token-EQUAL)]
   ["<=" (token-LE)]
   [">=" (token-GE)]
   ["!=" (token-NOTEQUAL)]
   ["&&" (token-AND)]
   ["||" (token-OR)]
   ["++" (token-INC)]
   ["--" (token-DEC)]
   ["+" (token-ADD)]
   ["-" (token-SUB)]
   ["*" (token-MUL)]
   ["/" (token-DIV)]
   ["&" (token-BITAND)]
   ["|" (token-BITOR)]
   ["^" (token-CARET)]
   ["%" (token-MOD)]
   ["+=" (token-ADD_ASSIGN)]
   ["-=" (token-SUB_ASSIGN)]
   ["*=" (token-MUL_ASSIGN)]
   ["/=" (token-DIV_ASSIGN)]
   ["&=" (token-AND_ASSIGN)]
   ["|=" (token-OR_ASSIGN)]
   ["^=" (token-XOR_ASSIGN)]
   ["%=" (token-MOD_ASSIGN)]
   ["<<=" (token-LSHIFT_ASSIGN)]
   [">>=" (token-RSHIFT_ASSIGN)]
   [">>>=" (token-URSHIFT_ASSIGN)]
   ["->" (token-ARROW)]
   ["::" (token-COLONCOLON)]
   ["@" (token-AT)]
   ["..." (token-ELLIPSIS)]
   [(:+ (char-set " \t\r\n\u000C")) (token-WS lexeme)]
   [(:: "/*" (:? (:* any-char)) "*/") (token-COMMENT lexeme)]
   [(:: "//" (:* (char-complement (char-set "\r\n")))) (token-LINE_COMMENT lexeme)]
   [(:: Letter (:* LetterOrDigit)) (token-IDENTIFIER lexeme)]
   [(eof) (token-EOF)]))

(define (channel-token? channel tok)
  (case channel
    [(default) (and (member (token-name (position-token-token tok))
                            '(WS COMMENT LINE_COMMENT))
                    #t)]
    [(hidden) #t]
    [else #f]))

(define (tokenize-java file #:channel [channel 'default])
  (let ([input (open-input-file file)])
    (let loop ([tokens '()])
      (let ([tok (java-lexer input)])
        (if (eq? (position-token-token tok) 'EOF)
            (reverse tokens)
            (loop (if (channel-token? channel tok)
                      tokens
                      (cons tok tokens))))))))

(provide empty-tokens
         tokens
         java-lexer
         tokenize-java)

(module+ test
  (require racket/pretty)
  (define-runtime-path example-dir "./example")
  (pretty-print (tokenize-java (build-path example-dir "Simple.java"))))