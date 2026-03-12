#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
;; ============================================================================
;; Java Lexer & Parser
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

   ;; Hex float literal: 0x...p...
   [(:: (:or "0x" "0X")
        (:+ (:or numeric #\_))
        (:or "." "")
        (:* (:or numeric #\_))
        (:or "p" "P")
        (:? (:or "+" "-"))
        (:+ numeric)
        (:? (:or "f" "F" "d" "D")))
    (token-HEX_FLOAT_LITERAL lexeme)]

   ;; Float literal with decimal point
   [(:: (:+ numeric)
        #\.
        (:* numeric)
        (:? (:: (:or "e" "E") (:? (:or "+" "-")) (:+ numeric)))
        (:? (:or "f" "F" "d" "D")))
    (token-FLOAT_LITERAL lexeme)]

   ;; Float literal starting with decimal point
   [(:: #\.
        (:+ numeric)
        (:? (:: (:or "e" "E") (:? (:or "+" "-")) (:+ numeric)))
        (:? (:or "f" "F" "d" "D")))
    (token-FLOAT_LITERAL lexeme)]

   ;; Float literal with exponent only
   [(:: (:+ numeric)
        (:or "e" "E")
        (:? (:or "+" "-"))
        (:+ numeric)
        (:? (:or "f" "F" "d" "D")))
    (token-FLOAT_LITERAL lexeme)]

   ;; Float literal with suffix only
   [(:: (:+ numeric)
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
;; AST Node Structures
;; ----------------------------------------------------------------------------

;; Compilation unit
(struct ast-compilation-unit (package imports type-declarations) #:transparent)

;; Type declarations
(struct ast-class-decl (modifiers name type-params super interfaces permits members) #:transparent)
(struct ast-interface-decl (modifiers name type-params extends permits members) #:transparent)
(struct ast-enum-decl (modifiers name interfaces constants body) #:transparent)
(struct ast-record-decl (modifiers name type-params parameters implements members) #:transparent)

;; Members
(struct ast-field-decl (modifiers type declarators) #:transparent)
(struct ast-method-decl (modifiers type-params return-type name params throws body) #:transparent)
(struct ast-constructor-decl (modifiers type-params name params throws body) #:transparent)

;; Statements
(struct ast-block (statements) #:transparent)
(struct ast-if-stmt (condition then-part else-part) #:transparent)
(struct ast-while-stmt (condition body) #:transparent)
(struct ast-do-stmt (body condition) #:transparent)
(struct ast-for-stmt (init condition update body) #:transparent)
(struct ast-enhanced-for-stmt (modifiers type var expr body) #:transparent)
(struct ast-switch-stmt (expression cases) #:transparent)
(struct ast-switch-case (labels statements) #:transparent)
(struct ast-return-stmt (expr) #:transparent)
(struct ast-throw-stmt (expr) #:transparent)
(struct ast-try-stmt (body catches finally) #:transparent)
(struct ast-catch-clause (exception-type var body) #:transparent)
(struct ast-synchronized-stmt (lock body) #:transparent)
(struct ast-assert-stmt (condition detail) #:transparent)
(struct ast-break-stmt (label) #:transparent)
(struct ast-continue-stmt (label) #:transparent)
(struct ast-yield-stmt (expr) #:transparent)
(struct ast-expr-stmt (expr) #:transparent)
(struct ast-local-var-decl (modifiers type declarators) #:transparent)

;; Expressions
(struct ast-literal (value type) #:transparent)
(struct ast-identifier (name) #:transparent)
(struct ast-binary-expr (op left right) #:transparent)
(struct ast-unary-expr (op expr prefix?) #:transparent)
(struct ast-ternary-expr (condition then-part else-part) #:transparent)
(struct ast-assign-expr (op target value) #:transparent)
(struct ast-method-call (target name args) #:transparent)
(struct ast-field-access (target field) #:transparent)
(struct ast-array-access (array index) #:transparent)
(struct ast-new-object (type args body) #:transparent)
(struct ast-new-array (type dimensions initializer) #:transparent)
(struct ast-cast-expr (type expr) #:transparent)
(struct ast-instanceof-expr (expr type pattern) #:transparent)
(struct ast-lambda-expr (params body) #:transparent)
(struct ast-method-ref (type name) #:transparent)
(struct ast-annotation (name values) #:transparent)

;; Types
(struct ast-type (name type-args array-dims) #:transparent)
(struct ast-type-param (name bounds) #:transparent)

;; Variable declarator
(struct ast-var-declarator (name initializer array-dims) #:transparent)

;; Formal parameter
(struct ast-formal-param (modifiers type var-args? name) #:transparent)

;; ----------------------------------------------------------------------------
;; Parser
;; ----------------------------------------------------------------------------

(define java-parser
  (parser
   (start compilation-unit)
   (end EOF)
   (tokens literals keywords)
   (src-pos)
   (error (lambda (tok-ok? tok-name tok-value start end)
            (error 'java-parser "Parse error at line ~a, col ~a: ~a ~a"
                   (position-line start) (position-col start)
                   tok-name tok-value)))

   (grammar
    ;; Compilation unit
    (compilation-unit
     [(package-decl? import-decl* type-decl*)
      (ast-compilation-unit $1 $2 $3)])

    (package-decl?
     [() #f]
     [(PACKAGE qualified-name SEMI) $2])

    (import-decl*
     [() '()]
     [(import-decl import-decl*) (cons $1 $2)])

    (import-decl
     [(IMPORT qualified-name SEMI) (list 'import $2 #f)]
     [(IMPORT qualified-name DOT MUL SEMI) (list 'import $2 'all)]
     [(IMPORT STATIC qualified-name SEMI) (list 'import-static $3 #f)]
     [(IMPORT STATIC qualified-name DOT MUL SEMI) (list 'import-static $3 'all)])

    (type-decl*
     [() '()]
     [(type-decl type-decl*) (cons $1 $2)]
     [(SEMI type-decl*) $2])

    (type-decl
     [(class-decl) $1]
     [(interface-decl) $1]
     [(enum-decl) $1]
     [(record-decl) $1]
     [(modifier* class-decl-no-mods)
      (struct-copy ast-class-decl $2 [modifiers (append $1 (ast-class-decl-modifiers $2))])]
     [(modifier* interface-decl-no-mods)
      (struct-copy ast-interface-decl $2 [modifiers (append $1 (ast-interface-decl-modifiers $2))])]
     [(modifier* enum-decl-no-mods)
      (struct-copy ast-enum-decl $2 [modifiers (append $1 (ast-enum-decl-modifiers $2))])]
     [(modifier* record-decl-no-mods)
      (struct-copy ast-record-decl $2 [modifiers (append $1 (ast-record-decl-modifiers $2))])])

    ;; Modifiers
    (modifier*
     [() '()]
     [(modifier modifier*) (cons $1 $2)])

    (modifier
     [(PUBLIC) 'public]
     [(PROTECTED) 'protected]
     [(PRIVATE) 'private]
     [(STATIC) 'static]
     [(ABSTRACT) 'abstract]
     [(FINAL) 'final]
     [(SEALED) 'sealed]
     [(NON_SEALED) 'non-sealed]
     [(STRICTFP) 'strictfp]
     [(NATIVE) 'native]
     [(SYNCHRONIZED) 'synchronized]
     [(TRANSIENT) 'transient]
     [(VOLATILE) 'volatile])

    ;; Class declaration
    (class-decl
     [(CLASS IDENTIFIER class-body)
      (ast-class-decl '() $2 '() #f '() '() $3)]
     [(CLASS IDENTIFIER EXTENDS type class-body)
      (ast-class-decl '() $2 '() $4 '() '() $5)]
     [(CLASS IDENTIFIER IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 '() #f $4 '() $5)]
     [(CLASS IDENTIFIER EXTENDS type IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 '() $4 $6 '() $7)]
     [(CLASS IDENTIFIER IMPLEMENTS type-list PERMITS type-list class-body)
      (ast-class-decl '() $2 '() #f $4 $6 $7)]
     [(CLASS IDENTIFIER EXTENDS type IMPLEMENTS type-list PERMITS type-list class-body)
      (ast-class-decl '() $2 '() $4 $6 $8 $9)])

    (class-decl-no-mods
     [(CLASS IDENTIFIER class-body)
      (ast-class-decl '() $2 '() #f '() '() $3)]
     [(CLASS IDENTIFIER EXTENDS type class-body)
      (ast-class-decl '() $2 '() $4 '() '() $5)]
     [(CLASS IDENTIFIER IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 '() #f $4 '() $5)]
     [(CLASS IDENTIFIER EXTENDS type IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 '() $4 $6 '() $7)])

    (class-body
     [(LBRACE class-body-decl* RBRACE) $2])

    (class-body-decl*
     [() '()]
     [(class-body-decl class-body-decl*) (append $1 $2)])

    (class-body-decl
     [(SEMI) '()]
     [(field-decl) (list $1)]
     [(method-decl) (list $1)]
     [(constructor-decl) (list $1)]
     [(STATIC block) (list (ast-block $2))]
     [(block) (list (ast-block $1))])

    ;; Interface declaration
    (interface-decl
     [(INTERFACE IDENTIFIER interface-body)
      (ast-interface-decl '() $2 '() '() '() $3)]
     [(INTERFACE IDENTIFIER EXTENDS type-list interface-body)
      (ast-interface-decl '() $2 '() $4 '() $5)]
     [(INTERFACE IDENTIFIER PERMITS type-list interface-body)
      (ast-interface-decl '() $2 '() '() $4 $5)])

    (interface-decl-no-mods
     [(INTERFACE IDENTIFIER interface-body)
      (ast-interface-decl '() $2 '() '() '() $3)]
     [(INTERFACE IDENTIFIER EXTENDS type-list interface-body)
      (ast-interface-decl '() $2 '() $4 '() $5)])

    (interface-body
     [(LBRACE interface-body-decl* RBRACE) $2])

    (interface-body-decl*
     [() '()]
     [(interface-body-decl interface-body-decl*) (append $1 $2)])

    (interface-body-decl
     [(SEMI) '()]
     [(field-decl) (list $1)]
     [(interface-method-decl) (list $1)])

    (interface-method-decl
     [(type IDENTIFIER LPAREN param-list? RPAREN SEMI)
      (ast-method-decl '() '() $1 $2 $4 '() #f)]
     [(type IDENTIFIER LPAREN param-list? RPAREN DEFAULT block)
      (ast-method-decl '() '() $1 $2 $4 '() $7)])

    ;; Enum declaration
    (enum-decl
     [(ENUM IDENTIFIER LBRACE enum-constants? enum-body-decls? RBRACE)
      (ast-enum-decl '() $2 '() $4 $5)]
     [(ENUM IDENTIFIER IMPLEMENTS type-list LBRACE enum-constants? enum-body-decls? RBRACE)
      (ast-enum-decl '() $2 $4 $6 $7)])

    (enum-decl-no-mods
     [(ENUM IDENTIFIER LBRACE enum-constants? enum-body-decls? RBRACE)
      (ast-enum-decl '() $2 '() $4 $5)])

    (enum-constants?
     [() '()]
     [(enum-constants) $1])

    (enum-constants
     [(enum-constant) (list $1)]
     [(enum-constant COMMA enum-constants) (cons $1 $3)])

    (enum-constant
     [(IDENTIFIER) $1]
     [(IDENTIFIER LPAREN arg-list? RPAREN) (list $1 $3)]
     [(IDENTIFIER LPAREN arg-list? RPAREN class-body) (list $1 $3 $5)])

    (enum-body-decls?
     [() '()]
     [(SEMI class-body-decl*) $2])

    ;; Record declaration (Java 14+)
    (record-decl
     [(RECORD IDENTIFIER LPAREN record-components? RPAREN record-body)
      (ast-record-decl '() $2 '() $4 '() $6)]
     [(RECORD IDENTIFIER LPAREN record-components? RPAREN IMPLEMENTS type-list record-body)
      (ast-record-decl '() $2 '() $4 $7 $8)])

    (record-decl-no-mods
     [(RECORD IDENTIFIER LPAREN record-components? RPAREN record-body)
      (ast-record-decl '() $2 '() $4 '() $6)])

    (record-components?
     [() '()]
     [(record-components) $1])

    (record-components
     [(record-component) (list $1)]
     [(record-component COMMA record-components) (cons $1 $3)])

    (record-component
     [(type IDENTIFIER) (ast-formal-param '() $1 #f $2)])

    (record-body
     [(LBRACE record-body-decl* RBRACE) $2])

    (record-body-decl*
     [() '()]
     [(record-body-decl record-body-decl*) (append $1 $2)])

    (record-body-decl
     [(SEMI) '()]
     [(field-decl) (list $1)]
     [(method-decl) (list $1)]
     [(constructor-decl) (list $1)])

    ;; Field declaration
    (field-decl
     [(type var-declarators SEMI) (ast-field-decl '() $1 $2)]
     [(modifier* type var-declarators SEMI) (ast-field-decl $1 $2 $3)])

    (var-declarators
     [(var-declarator) (list $1)]
     [(var-declarator COMMA var-declarators) (cons $1 $3)])

    (var-declarator
     [(IDENTIFIER) (ast-var-declarator $1 #f 0)]
     [(IDENTIFIER ASSIGN expr) (ast-var-declarator $1 $3 0)]
     [(IDENTIFIER array-dims) (ast-var-declarator $1 #f $2)]
     [(IDENTIFIER array-dims ASSIGN expr) (ast-var-declarator $1 $4 $2)])

    (array-dims
     [(LBRACK RBRACK) 1]
     [(LBRACK RBRACK array-dims) (+ 1 $3)])

    ;; Method declaration
    (method-decl
     [(type IDENTIFIER LPAREN param-list? RPAREN method-body)
      (ast-method-decl '() '() $1 $2 $4 '() $6)]
     [(type IDENTIFIER LPAREN param-list? RPAREN THROWS type-list method-body)
      (ast-method-decl '() '() $1 $2 $4 $7 $8)]
     [(VOID IDENTIFIER LPAREN param-list? RPAREN method-body)
      (ast-method-decl '() '() 'void $2 $4 '() $6)]
     [(VOID IDENTIFIER LPAREN param-list? RPAREN THROWS type-list method-body)
      (ast-method-decl '() '() 'void $2 $4 $7 $8)]
     [(modifier* type IDENTIFIER LPAREN param-list? RPAREN method-body)
      (ast-method-decl $1 '() $2 $3 $5 '() $7)]
     [(modifier* type IDENTIFIER LPAREN param-list? RPAREN THROWS type-list method-body)
      (ast-method-decl $1 '() $2 $3 $5 $8 $9)]
     [(modifier* VOID IDENTIFIER LPAREN param-list? RPAREN method-body)
      (ast-method-decl $1 '() 'void $3 $5 '() $7)]
     [(modifier* VOID IDENTIFIER LPAREN param-list? RPAREN THROWS type-list method-body)
      (ast-method-decl $1 '0 'void $3 $5 $8 $9)])

    (method-body
     [(block) $1]
     [(SEMI) #f])

    ;; Constructor declaration
    (constructor-decl
     [(IDENTIFIER LPAREN param-list? RPAREN block)
      (ast-constructor-decl '() '() $1 $3 '() $5)]
     [(IDENTIFIER LPAREN param-list? RPAREN THROWS type-list block)
      (ast-constructor-decl '() '() $1 $3 $6 $7)]
     [(modifier* IDENTIFIER LPAREN param-list? RPAREN block)
      (ast-constructor-decl $1 '() $2 $4 '() $6)]
     [(modifier* IDENTIFIER LPAREN param-list? RPAREN THROWS type-list block)
      (ast-constructor-decl $1 '() $2 $4 $7 $8)])

    ;; Parameter list
    (param-list?
     [() '()]
     [(param-list) $1])

    (param-list
     [(formal-param) (list $1)]
     [(formal-param COMMA param-list) (cons $1 $3)])

    (formal-param
     [(type IDENTIFIER) (ast-formal-param '() $1 #f $2)]
     [(type ELLIPSIS IDENTIFIER) (ast-formal-param '() $1 #t $3)]
     [(FINAL type IDENTIFIER) (ast-formal-param '(final) $2 #f $3)]
     [(FINAL type ELLIPSIS IDENTIFIER) (ast-formal-param '(final) $2 #t $4)])

    ;; Type
    (type
     [(primitive-type) (ast-type $1 '() 0)]
     [(primitive-type array-dims) (ast-type $1 '() $2)]
     [(qualified-name) (ast-type $1 '() 0)]
     [(qualified-name type-args) (ast-type $1 $2 0)]
     [(qualified-name array-dims) (ast-type $1 '() $2)])

    (primitive-type
     [(BOOLEAN) 'boolean]
     [(BYTE) 'byte]
     [(SHORT) 'short]
     [(INT) 'int]
     [(LONG) 'long]
     [(CHAR) 'char]
     [(FLOAT) 'float]
     [(DOUBLE) 'double]
     [(VAR) 'var])

    (type-args
     [(LT type-list GT) $2])

    (type-list
     [(type) (list $1)]
     [(type COMMA type-list) (cons $1 $3)])

    ;; Qualified name
    (qualified-name
     [(IDENTIFIER) $1]
     [(IDENTIFIER DOT qualified-name) (string-append $1 "." $3)])

    ;; Block and statements
    (block
     [(LBRACE block-stmt* RBRACE) $2])

    (block-stmt*
     [() '()]
     [(block-stmt block-stmt*) (cons $1 $2)])

    (block-stmt
     [(stmt) $1]
     [(local-var-decl) $1]
     [(type-decl) $1])

    (local-var-decl
     [(type var-declarators SEMI) (ast-local-var-decl '() $1 $2)]
     [(FINAL type var-declarators SEMI) (ast-local-var-decl '(final) $2 $3)]
     [(VAR IDENTIFIER ASSIGN expr SEMI) (ast-local-var-decl '() 'var (list (ast-var-declarator $2 $4 0)))])

    ;; Statements
    (stmt
     [(block) (ast-block $1)]
     [(IF LPAREN expr RPAREN stmt) (ast-if-stmt $3 $5 #f)]
     [(IF LPAREN expr RPAREN stmt ELSE stmt) (ast-if-stmt $3 $5 $7)]
     [(WHILE LPAREN expr RPAREN stmt) (ast-while-stmt $3 $5)]
     [(DO stmt WHILE LPAREN expr RPAREN SEMI) (ast-do-stmt $2 $5)]
     [(FOR LPAREN for-control RPAREN stmt) $3]
     [(SWITCH LPAREN expr RPAREN LBRACE switch-case* RBRACE)
      (ast-switch-stmt $3 $6)]
     [(RETURN SEMI) (ast-return-stmt #f)]
     [(RETURN expr SEMI) (ast-return-stmt $2)]
     [(THROW expr SEMI) (ast-throw-stmt $2)]
     [(TRY block catch-clause+ finally-clause) (ast-try-stmt $2 $3 $4)]
     [(TRY block finally-clause) (ast-try-stmt $2 '() $3)]
     [(SYNCHRONIZED LPAREN expr RPAREN block) (ast-synchronized-stmt $3 $5)]
     [(ASSERT expr SEMI) (ast-assert-stmt $2 #f)]
     [(ASSERT expr COLON expr SEMI) (ast-assert-stmt $2 $4)]
     [(BREAK SEMI) (ast-break-stmt #f)]
     [(BREAK IDENTIFIER SEMI) (ast-break-stmt $2)]
     [(CONTINUE SEMI) (ast-continue-stmt #f)]
     [(CONTINUE IDENTIFIER SEMI) (ast-continue-stmt $2)]
     [(YIELD expr SEMI) (ast-yield-stmt $2)]
     [(SEMI) (ast-block '())]
     [(expr SEMI) (ast-expr-stmt $1)])

    (catch-clause+
     [(catch-clause) (list $1)]
     [(catch-clause catch-clause+) (cons $1 $2)])

    (catch-clause
     [(CATCH LPAREN type IDENTIFIER RPAREN block)
      (ast-catch-clause $3 $4 $6)]
     [(CATCH LPAREN type BITOR type-list IDENTIFIER RPAREN block)
      (ast-catch-clause (cons $3 $5) $6 $8)])

    (finally-clause
     [() #f]
     [(FINALLY block) $2])

    ;; For control
    (for-control
     [(for-init? SEMI expr? SEMI expr-list? stmt)
      (ast-for-stmt $1 $3 $5 $6)]
     [(FINAL type IDENTIFIER COLON expr stmt)
      (ast-enhanced-for-stmt '(final) $2 $3 $5 $6)]
     [(type IDENTIFIER COLON expr stmt)
      (ast-enhanced-for-stmt '() $1 $2 $4 $5)])

    (for-init?
     [() #f]
     [(for-init) $1])

    (for-init
     [(local-var-decl) $1]
     [(expr-list) $1])

    ;; Switch cases
    (switch-case*
     [() '()]
     [(switch-case switch-case*) (cons $1 $2)])

    (switch-case
     [(CASE expr COLON block-stmt*) (ast-switch-case (list $2) $4)]
     [(DEFAULT COLON block-stmt*) (ast-switch-case '(default) $3)]
     [(CASE expr COLON switch-case) $4])

    ;; Expressions
    (expr-list
     [(expr) (list $1)]
     [(expr COMMA expr-list) (cons $1 $3)])

    (expr-list?
     [() '()]
     [(expr-list) $1])

    (arg-list?
     [() '()]
     [(expr-list) $1])

    ;; Expression with precedence (simplified - not full precedence levels)
    (expr
     [(assignment-expr) $1])

    (assignment-expr
     [(ternary-expr) $1]
     [(ternary-expr ASSIGN assignment-expr) (ast-assign-expr '= $1 $3)]
     [(ternary-expr ADD_ASSIGN assignment-expr) (ast-assign-expr '+= $1 $3)]
     [(ternary-expr SUB_ASSIGN assignment-expr) (ast-assign-expr '-= $1 $3)]
     [(ternary-expr MUL_ASSIGN assignment-expr) (ast-assign-expr '*= $1 $3)]
     [(ternary-expr DIV_ASSIGN assignment-expr) (ast-assign-expr '/= $1 $3)])

    (ternary-expr
     [(or-expr) $1]
     [(or-expr QUESTION expr COLON ternary-expr) (ast-ternary-expr $1 $3 $5)])

    (or-expr
     [(and-expr) $1]
     [(or-expr OR and-expr) (ast-binary-expr '|| $1 $3)])

    (and-expr
     [(bitor-expr) $1]
     [(and-expr AND bitor-expr) (ast-binary-expr '&& $1 $3)])

    (bitor-expr
     [(bitxor-expr) $1]
     [(bitor-expr BITOR bitxor-expr) (ast-binary-expr '|bor| $1 $3)])

    (bitxor-expr
     [(bitand-expr) $1]
     [(bitxor-expr CARET bitand-expr) (ast-binary-expr '^ $1 $3)])

    (bitand-expr
     [(equality-expr) $1]
     [(bitand-expr BITAND equality-expr) (ast-binary-expr '& $1 $3)])

    (equality-expr
     [(relational-expr) $1]
     [(equality-expr EQUAL relational-expr) (ast-binary-expr '== $1 $3)]
     [(equality-expr NOTEQUAL relational-expr) (ast-binary-expr '!= $1 $3)])

    (relational-expr
     [(shift-expr) $1]
     [(relational-expr LT shift-expr) (ast-binary-expr '< $1 $3)]
     [(relational-expr GT shift-expr) (ast-binary-expr '> $1 $3)]
     [(relational-expr LE shift-expr) (ast-binary-expr '<= $1 $3)]
     [(relational-expr GE shift-expr) (ast-binary-expr '>= $1 $3)]
     [(relational-expr INSTANCEOF type) (ast-instanceof-expr $1 $3 #f)]
     [(relational-expr INSTANCEOF type IDENTIFIER) (ast-instanceof-expr $1 $3 $4)])

    (shift-expr
     [(additive-expr) $1]
     [(shift-expr LSHIFT additive-expr) (ast-binary-expr '<< $1 $3)]
     [(shift-expr RSHIFT additive-expr) (ast-binary-expr '>> $1 $3)]
     [(shift-expr URSHIFT additive-expr) (ast-binary-expr '>>> $1 $3)])

    (additive-expr
     [(multiplicative-expr) $1]
     [(additive-expr ADD multiplicative-expr) (ast-binary-expr '+ $1 $3)]
     [(additive-expr SUB multiplicative-expr) (ast-binary-expr '- $1 $3)])

    (multiplicative-expr
     [(unary-expr) $1]
     [(multiplicative-expr MUL unary-expr) (ast-binary-expr '* $1 $3)]
     [(multiplicative-expr DIV unary-expr) (ast-binary-expr '/ $1 $3)]
     [(multiplicative-expr MOD unary-expr) (ast-binary-expr '% $1 $3)])

    (unary-expr
     [(pre-inc-expr) $1]
     [(ADD unary-expr) (ast-unary-expr '+ $2 #t)]
     [(SUB unary-expr) (ast-unary-expr '- $2 #t)]
     [(INC unary-expr) (ast-unary-expr '++ $2 #t)]
     [(DEC unary-expr) (ast-unary-expr '-- $2 #t)]
     [(BANG unary-expr) (ast-unary-expr '! $2 #t)]
     [(TILDE unary-expr) (ast-unary-expr '~ $2 #t)])

    (pre-inc-expr
     [(postfix-expr) $1])

    (postfix-expr
     [(primary) $1]
     [(postfix-expr INC) (ast-unary-expr '++ $1 #f)]
     [(postfix-expr DEC) (ast-unary-expr '-- $1 #f)])

    (primary
     [(LPAREN expr RPAREN) $2]
     [(literal) $1]
     [(THIS) (ast-identifier 'this)]
     [(SUPER) (ast-identifier 'super)]
     [(IDENTIFIER) (ast-identifier $1)]
     [(primary DOT IDENTIFIER) (ast-field-access $1 $3)]
     [(primary DOT THIS) (ast-field-access $1 'this)]
     [(primary DOT SUPER) (ast-field-access $1 'super)]
     [(primary LPAREN arg-list? RPAREN) (ast-method-call #f $1 $3)]
     [(IDENTIFIER LPAREN arg-list? RPAREN) (ast-method-call #f $1 $3)]
     [(primary LBRACK expr RBRACK) (ast-array-access $1 $3)]
     [(NEW creator) $2]
     [(LPAREN type RPAREN unary-expr) (ast-cast-expr $2 $4)])

    (creator
     [(IDENTIFIER LPAREN arg-list? RPAREN) (ast-new-object $1 $3 #f)]
     [(IDENTIFIER LPAREN arg-list? RPAREN class-body) (ast-new-object $1 $3 $5)]
     [(primitive-type array-creator) (ast-new-array $1 (car $2) (cdr $2))])

    (array-creator
     [(LBRACK RBRACK array-dims) (cons '() $3)]
     [(LBRACK expr RBRACK array-dims?) (cons $2 (or $4 0))]
     [(LBRACK RBRACK LBRACE array-init? RBRACE) (cons '() $4)])

    (array-dims?
     [() 0]
     [(array-dims) $1])

    (array-init?
     [() '()]
     [(array-init) $1])

    (array-init
     [(expr) (list $1)]
     [(expr COMMA) (list $1)]
     [(expr COMMA array-init) (cons $1 $3)])

    ;; Literals
    (literal
     [(DECIMAL_LITERAL) (ast-literal (string->number $1) 'int)]
     [(HEX_LITERAL) (ast-literal (string->number $1) 'int)]
     [(OCT_LITERAL) (ast-literal (string->number $1 8) 'int)]
     [(BINARY_LITERAL) (ast-literal (string->number $1 2) 'int)]
     [(FLOAT_LITERAL) (ast-literal (string->number $1) 'float)]
     [(HEX_FLOAT_LITERAL) (ast-literal (string->number $1) 'float)]
     [(BOOL_LITERAL) (ast-literal $1 'boolean)]
     [(CHAR_LITERAL) (ast-literal $1 'char)]
     [(STRING_LITERAL) (ast-literal $1 'string)]
     [(TEXT_BLOCK) (ast-literal $1 'text-block)]
     [(NULL_LITERAL) (ast-literal 'null 'null)])

    ;; Optional expressions
    (expr?
     [() #f]
     [(expr) $1]))))

;; ----------------------------------------------------------------------------
;; Helper Functions
;; ----------------------------------------------------------------------------

;; Lexing helper
(define (lex-this lexer input)
  (lambda ()
    (lexer input)))

;; Parse Java code string
(define (parse-java-code code-string)
  (let ([input (open-input-string code-string)])
    (port-count-lines! input)
    (java-parser (lex-this java-lexer input))))

;; Tokenize Java code
(define (tokenize-java code-string)
  (let ([input (open-input-string code-string)])
    (port-count-lines! input)
    (let loop ([tokens '()])
      (let ([tok (java-lexer input)])
        (if (eq? (position-token-token tok) 'EOF)
            (reverse tokens)
            (loop (cons tok tokens)))))))

;; ----------------------------------------------------------------------------
;; Example
;; ----------------------------------------------------------------------------

(define sample-java-code
  "public class HelloWorld {
    int x;
    int y = 10;
    private String message = \"Hello, World!\";

    public void main() {
        if (x > 0) {
            x = x + 1;
        }
        while (x < 100) {
            x = x + y;
        }
    }

    public int calculate() {
        return x + y;
    }

    public static void staticMethod() {
        var local = 42;
    }
}")

;; Main function
(define (main)
  (displayln "=== Java Lexer & Parser (Based on ANTLR Java Grammar) ===")
  (displayln "")

  (displayln "--- Lexical Analysis ---")
  (displayln "Tokens:")
  (for ([tok (tokenize-java sample-java-code)])
    (let ([token (position-token-token tok)]
          [start-pos (position-token-start-pos tok)])
      (displayln (format "  ~a at line ~a, col ~a"
                         (if (list? token) (car token) token)
                         (position-line start-pos)
                         (position-col start-pos)))))

  (displayln "")
  (displayln "--- Syntax Analysis ---")
  (displayln "AST:")
  (pretty-print (parse-java-code sample-java-code)))

;; Run
(main)
