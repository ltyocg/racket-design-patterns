#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "lexer.rkt")

;; ============================================================================
;; Java Parser
;; Based on ANTLR Java grammar: https://github.com/antlr/grammars-v4/tree/master/java/java
;; Supports Java 17 features including records, sealed classes, pattern matching, etc.
;; ============================================================================

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
   (tokens empty-tokens tokens)
   (src-pos)
   (error (lambda (tok-ok? tok-name tok-value start end)
            (error 'java-parser "Parse error at line ~a, col ~a: ~a ~a"
                   (position-line start) (position-col start)
                   tok-name tok-value)))

   ;; Precedence declarations - lower number = lower precedence
   ;; This helps resolve shift/reduce conflicts
   (precs
    (left INC DEC)           ; postfix inc/dec
    (right ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
           AND_ASSIGN OR_ASSIGN XOR_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN URSHIFT_ASSIGN)
    (right QUESTION COLON)
    (left OR)
    (left AND)
    (left BITOR)
    (left CARET)
    (left BITAND)
    (left EQUAL NOTEQUAL)
    (left LT GT LE GE INSTANCEOF)
    (left LSHIFT RSHIFT URSHIFT)
    (left ADD SUB)
    (left MUL DIV MOD)
    (right BANG TILDE)
    (left DOT LBRACK LPAREN))

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
     ;; Static import with wildcard: import static java.lang.System.*;
     [(IMPORT STATIC import-wildcard-path SEMI)
      (list 'import-static $3 'all)]
     ;; Static import without wildcard: import static java.lang.System.out;
     [(IMPORT STATIC import-path SEMI)
      (list 'import-static $3 #f)]
     ;; Import with wildcard: import java.util.*;
     [(IMPORT import-wildcard-path SEMI)
      (list 'import $2 'all)]
     ;; Import without wildcard: import java.util.Random;
     [(IMPORT import-path SEMI)
      (list 'import $2 #f)])

    ;; Import wildcard path - ends with .*
    ;; Handles: java.util.* -> returns "java.util"
    (import-wildcard-path
     [(IDENTIFIER DOT MUL) $1]
     [(IDENTIFIER DOT import-wildcard-path) (string-append $1 "." $3)])

    ;; Import path - similar to qualified-name but used specifically for imports
    (import-path
     [(IDENTIFIER) $1]
     [(IDENTIFIER DOT import-path) (string-append $1 "." $3)])

    (type-decl*
     [() '()]
     [(type-decl type-decl*) (cons $1 $2)]
     [(SEMI type-decl*) $2])

    (type-decl
     [(class-decl) $1]
     [(interface-decl) $1]
     [(enum-decl) $1]
     [(record-decl) $1]
     [(annotation-decl) $1]
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
     [(CLASS IDENTIFIER type-params class-body)
      (ast-class-decl '() $2 $3 #f '() '() $4)]
     [(CLASS IDENTIFIER EXTENDS type class-body)
      (ast-class-decl '() $2 '() $4 '() '() $5)]
     [(CLASS IDENTIFIER type-params EXTENDS type class-body)
      (ast-class-decl '() $2 $3 $5 '() '() $6)]
     [(CLASS IDENTIFIER IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 '() #f $4 '() $5)]
     [(CLASS IDENTIFIER type-params IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 $3 #f $5 '() $6)]
     [(CLASS IDENTIFIER EXTENDS type IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 '() $4 $6 '() $7)]
     [(CLASS IDENTIFIER type-params EXTENDS type IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 $3 $5 $7 '() $8)]
     [(CLASS IDENTIFIER IMPLEMENTS type-list PERMITS type-list class-body)
      (ast-class-decl '() $2 '() #f $4 $6 $7)]
     [(CLASS IDENTIFIER EXTENDS type IMPLEMENTS type-list PERMITS type-list class-body)
      (ast-class-decl '() $2 '() $4 $6 $8 $9)])

    (class-decl-no-mods
     [(CLASS IDENTIFIER class-body)
      (ast-class-decl '() $2 '() #f '() '() $3)]
     [(CLASS IDENTIFIER type-params class-body)
      (ast-class-decl '() $2 $3 #f '() '() $4)]
     [(CLASS IDENTIFIER EXTENDS type class-body)
      (ast-class-decl '() $2 '() $4 '() '() $5)]
     [(CLASS IDENTIFIER type-params EXTENDS type class-body)
      (ast-class-decl '() $2 $3 $5 '() '() $6)]
     [(CLASS IDENTIFIER IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 '() #f $4 '() $5)]
     [(CLASS IDENTIFIER type-params IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 $3 #f $5 '() $6)]
     [(CLASS IDENTIFIER EXTENDS type IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 '() $4 $6 '() $7)]
     [(CLASS IDENTIFIER type-params EXTENDS type IMPLEMENTS type-list class-body)
      (ast-class-decl '() $2 $3 $5 $7 '() $8)])

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
     [(block) (list (ast-block $1))]
     [(class-decl) (list $1)]
     [(interface-decl) (list $1)]
     [(enum-decl) (list $1)]
     [(modifier* class-decl-no-mods) (list $2)]
     [(modifier* interface-decl-no-mods) (list $2)]
     [(modifier* enum-decl-no-mods) (list $2)])

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
     [(VOID IDENTIFIER LPAREN param-list? RPAREN SEMI)
      (ast-method-decl '() '() 'void $2 $4 '() #f)]
     [(type IDENTIFIER LPAREN param-list? RPAREN DEFAULT block)
      (ast-method-decl '() '() $1 $2 $4 '() $7)]
     [(VOID IDENTIFIER LPAREN param-list? RPAREN DEFAULT block)
      (ast-method-decl '() '() 'void $2 $4 '() $7)])

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

    ;; Annotation declaration (@interface)
    (annotation-decl
     [(AT INTERFACE IDENTIFIER annotation-body)
      (ast-interface-decl '(annotation) $3 '() '() '() $4)])

    (annotation-body
     [(LBRACE annotation-body-decl* RBRACE) $2])

    (annotation-body-decl*
     [() '()]
     [(annotation-body-decl annotation-body-decl*) (append $1 $2)])

    (annotation-body-decl
     [(SEMI) '()]
     [(type IDENTIFIER LPAREN RPAREN SEMI)
      (list (ast-method-decl '() '() $1 $2 '() '() #f))]
     [(type IDENTIFIER LPAREN RPAREN DEFAULT expr SEMI)
      (list (ast-method-decl '() '() $1 $2 '() '() #f))])

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
      (ast-method-decl $1 '() 'void $3 $5 $8 $9)])

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

    ;; Type - note: array-dims only matches empty brackets [], not [expr]
    ;; We handle class-or-interface-type separately to avoid conflicts with array access
    (type
     [(primitive-type) (ast-type $1 '() 0)]
     [(primitive-type array-dims) (ast-type $1 '() $2)]
     [(class-or-interface-type) (ast-type $1 '() 0)]
     [(class-or-interface-type type-args) (ast-type $1 $2 0)]
     [(class-or-interface-type array-dims) (ast-type $1 '() $2)])

    ;; Class or interface type - similar to qualified-name but only used in type contexts
    ;; This avoids reduce/reduce conflicts with array access expressions
    (class-or-interface-type
     [(IDENTIFIER) $1]
     [(IDENTIFIER DOT class-or-interface-type) (string-append $1 "." $3)])

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

    (type-params
     [(LT type-param-list GT) $2])

    (type-param-list
     [(type-param) (list $1)]
     [(type-param COMMA type-param-list) (cons $1 $3)])

    (type-param
     [(IDENTIFIER) (ast-type-param $1 '())]
     [(IDENTIFIER EXTENDS type) (ast-type-param $1 (list $3))]
     [(IDENTIFIER EXTENDS type BITAND type-list) (ast-type-param $1 (cons $3 $5))])

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
     ;; Primitive type declarations - unambiguous because primitive keywords can't start expressions
     [(primitive-type array-dims var-declarators SEMI) (ast-local-var-decl '() (ast-type $1 '() $2) $3)]
     [(primitive-type var-declarators SEMI) (ast-local-var-decl '() (ast-type $1 '() 0) $2)]
     [(FINAL primitive-type array-dims var-declarators SEMI) (ast-local-var-decl '(final) (ast-type $2 '() $3) $4)]
     [(FINAL primitive-type var-declarators SEMI) (ast-local-var-decl '(final) (ast-type $2 '() 0) $3)]
     ;; Class type with array dimensions: String[] x = ...
     ;; This is unambiguous because [] must be empty for type declarations
     [(IDENTIFIER array-dims var-declarators SEMI) (ast-local-var-decl '() (ast-type $1 '() $2) $3)]
     [(FINAL IDENTIFIER array-dims var-declarators SEMI) (ast-local-var-decl '(final) (ast-type $2 '() $3) $4)]
     ;; Qualified type with array dimensions: java.lang.String[] x = ...
     [(qualified-type-name array-dims var-declarators SEMI) (ast-local-var-decl '() (ast-type $1 '() $2) $3)]
     [(FINAL qualified-type-name array-dims var-declarators SEMI) (ast-local-var-decl '(final) (ast-type $2 '() $3) $4)]
     ;; Simple class type declarations - IDENTIFIER followed by IDENTIFIER (name then variable)
     ;; This pattern: TypeName varName; or TypeName varName = value;
     [(IDENTIFIER IDENTIFIER SEMI) (ast-local-var-decl '() (ast-type $1 '() 0) (list (ast-var-declarator $2 #f 0)))]
     [(IDENTIFIER IDENTIFIER ASSIGN expr SEMI) (ast-local-var-decl '() (ast-type $1 '() 0) (list (ast-var-declarator $2 $4 0)))]
     [(FINAL IDENTIFIER IDENTIFIER SEMI) (ast-local-var-decl '(final) (ast-type $2 '() 0) (list (ast-var-declarator $3 #f 0)))]
     [(FINAL IDENTIFIER IDENTIFIER ASSIGN expr SEMI) (ast-local-var-decl '(final) (ast-type $2 '() 0) (list (ast-var-declarator $3 $5 0)))]
     ;; Qualified type name declarations: java.lang.String x = ...
     [(qualified-type-name IDENTIFIER SEMI) (ast-local-var-decl '() (ast-type $1 '() 0) (list (ast-var-declarator $2 #f 0)))]
     [(qualified-type-name IDENTIFIER ASSIGN expr SEMI) (ast-local-var-decl '() (ast-type $1 '() 0) (list (ast-var-declarator $2 $4 0)))]
     [(FINAL qualified-type-name IDENTIFIER SEMI) (ast-local-var-decl '(final) (ast-type $2 '() 0) (list (ast-var-declarator $3 #f 0)))]
     [(FINAL qualified-type-name IDENTIFIER ASSIGN expr SEMI) (ast-local-var-decl '(final) (ast-type $2 '() 0) (list (ast-var-declarator $3 $5 0)))]
     ;; VAR type inference - unambiguous
     [(VAR IDENTIFIER ASSIGN expr SEMI) (ast-local-var-decl '() 'var (list (ast-var-declarator $2 $4 0)))]
     [(FINAL VAR IDENTIFIER ASSIGN expr SEMI) (ast-local-var-decl '(final) 'var (list (ast-var-declarator $3 $5 0)))])

    ;; Qualified type name for declarations - must have at least one DOT
    ;; This distinguishes it from simple IDENTIFIER (which could be an expression)
    (qualified-type-name
     [(IDENTIFIER DOT IDENTIFIER) (string-append $1 "." $3)]
     [(IDENTIFIER DOT qualified-type-name) (string-append $1 "." $3)])

    ;; Statements
    (stmt
     [(block) (ast-block $1)]
     [(IF LPAREN expr RPAREN stmt) (ast-if-stmt $3 $5 #f)]
     [(IF LPAREN expr RPAREN stmt ELSE stmt) (ast-if-stmt $3 $5 $7)]
     [(WHILE LPAREN expr RPAREN stmt) (ast-while-stmt $3 $5)]
     [(DO stmt WHILE LPAREN expr RPAREN SEMI) (ast-do-stmt $2 $5)]
     [(FOR LPAREN for-init? SEMI expr? SEMI expr-list? RPAREN stmt)
      (ast-for-stmt $3 $5 $7 $9)]
     [(FOR LPAREN FINAL type IDENTIFIER COLON expr RPAREN stmt)
      (ast-enhanced-for-stmt '(final) $4 $5 $7 $9)]
     [(FOR LPAREN type IDENTIFIER COLON expr RPAREN stmt)
      (ast-enhanced-for-stmt '() $3 $4 $6 $8)]
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

    (for-init?
     [() #f]
     [(for-init) $1])

    (for-init
     [(for-local-var-decl) $1]
     [(expr-list) $1])

    (for-local-var-decl
     [(type var-declarators) (ast-local-var-decl '() $1 $2)]
     [(FINAL type var-declarators) (ast-local-var-decl '(final) $2 $3)])

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
     [(ternary-expr DIV_ASSIGN assignment-expr) (ast-assign-expr '/= $1 $3)]
     [(ternary-expr MOD_ASSIGN assignment-expr) (ast-assign-expr '%= $1 $3)]
     [(ternary-expr AND_ASSIGN assignment-expr) (ast-assign-expr '&= $1 $3)]
     [(ternary-expr OR_ASSIGN assignment-expr) (ast-assign-expr 'bitwise-or= $1 $3)]
     [(ternary-expr XOR_ASSIGN assignment-expr) (ast-assign-expr '^= $1 $3)]
     [(ternary-expr LSHIFT_ASSIGN assignment-expr) (ast-assign-expr '<<= $1 $3)]
     [(ternary-expr RSHIFT_ASSIGN assignment-expr) (ast-assign-expr '>>= $1 $3)]
     [(ternary-expr URSHIFT_ASSIGN assignment-expr) (ast-assign-expr '>>>= $1 $3)])

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
     [(postfix-expr) $1]
     [(ADD unary-expr) (ast-unary-expr '+ $2 #t)]
     [(SUB unary-expr) (ast-unary-expr '- $2 #t)]
     [(INC unary-expr) (ast-unary-expr '++ $2 #t)]
     [(DEC unary-expr) (ast-unary-expr '-- $2 #t)]
     [(BANG unary-expr) (ast-unary-expr '! $2 #t)]
     [(TILDE unary-expr) (ast-unary-expr '~ $2 #t)])

    ;; Primary handles base expressions
    (primary
     [(LPAREN expr RPAREN) $2]
     [(literal) $1]
     [(THIS) (ast-identifier 'this)]
     [(SUPER) (ast-identifier 'super)]
     [(IDENTIFIER) (ast-identifier $1)]
     [(NEW creator) $2]
     [(LPAREN type RPAREN unary-expr) (ast-cast-expr $2 $4)])

    ;; Postfix-expr handles all trailing operations: field access, method call, array access, postfix inc/dec
    ;; Also handles qualified names as field access chains
    (postfix-expr
     [(primary) $1]
     [(IDENTIFIER LPAREN arg-list? RPAREN) (ast-method-call #f $1 $3)]
     [(IDENTIFIER DOT CLASS) (ast-field-access (ast-identifier $1) 'class)]
     [(IDENTIFIER DOT postfix-expr-tail) (build-field-access-chain (ast-identifier $1) $3)]
     [(postfix-expr DOT IDENTIFIER) (ast-field-access $1 $3)]
     [(postfix-expr DOT CLASS) (ast-field-access $1 'class)]
     [(postfix-expr DOT THIS) (ast-field-access $1 'this)]
     [(postfix-expr DOT SUPER) (ast-field-access $1 'super)]
     [(postfix-expr LPAREN arg-list? RPAREN) (ast-method-call #f $1 $3)]
     [(postfix-expr LBRACK expr RBRACK) (ast-array-access $1 $3)]
     [(postfix-expr INC) (ast-unary-expr '++ $1 #f)]
     [(postfix-expr DEC) (ast-unary-expr '-- $1 #f)])

    ;; Tail of a qualified name - helps with parsing
    (postfix-expr-tail
     [(IDENTIFIER) $1]
     [(IDENTIFIER DOT postfix-expr-tail) (cons $1 $3)])

    (creator
     [(IDENTIFIER LPAREN arg-list? RPAREN) (ast-new-object $1 $3 #f)]
     [(IDENTIFIER LPAREN arg-list? RPAREN class-body) (ast-new-object $1 $3 $5)]
     [(IDENTIFIER array-creator) (ast-new-array $1 (car $2) (cdr $2))]
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

;; Build a chain of field-access expressions from an identifier list
;; e.g., (build-field-access-chain base '("prePost")) => (ast-field-access base "prePost")
;; e.g., (build-field-access-chain base '("b" "c")) => (ast-field-access (ast-field-access base "b") "c")
(define (build-field-access-chain base identifiers)
  (if (string? identifiers)
      (ast-field-access base identifiers)
      (let loop ([result base]
                 [ids (if (list? identifiers) identifiers (list identifiers))])
        (if (null? ids)
            result
            (loop (ast-field-access result (car ids))
                  (cdr ids))))))

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

;; ----------------------------------------------------------------------------
;; Tests
;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; Test helper: parse and check no error (wraps code in a class)
  (define (parse-successfully code)
    (with-handlers ([exn:fail? (lambda (e)
                                 (displayln (format "Parse error: ~a" (exn-message e)))
                                 #f)])
      (parse-java-code code)
      #t))

  ;; Test helper: parse a class body statement
  (define (parse-class-stmt stmt)
    (parse-successfully (format "class Test { ~a }" stmt)))

  ;; Test helper: parse and return result
  (define (parse-result code)
    (with-handlers ([exn:fail? (lambda (e) (cons 'error (exn-message e)))])
      (cons 'success (parse-java-code code))))

  ;; ---- Basic Token Tests ----
  ;; Note: tokenizer doesn't include EOF in count, and whitespace is skipped

  (test-case "Basic tokenization"
    (check-equal? (length (tokenize-java "int x = 5;")) 5
                  "Simple statement should have 5 tokens (int, x, =, 5, ;)")
    (check-equal? (length (tokenize-java "public class Foo {}")) 5
                  "Simple class should have 5 tokens (public, class, Foo, {, })"))

  ;; ---- Numeric Literal Tests (inside a class) ----

  (test-case "Numeric literals from AllInOne7.java"
    (check-true (parse-class-stmt "long creditCardNumber = 1234_5678_9012_3456L;"))
    (check-true (parse-class-stmt "long socialSecurityNumber = 999_99_9999L;"))
    (check-true (parse-class-stmt "float pi = 3.14_15F;"))
    (check-true (parse-class-stmt "long hexBytes = 0xFF_EC_DE_5E;"))
    (check-true (parse-class-stmt "long hexWords = 0xCAFE_BABE;"))
    (check-true (parse-class-stmt "long maxLong = 0x7fff_ffff_ffff_ffffL;"))
    (check-true (parse-class-stmt "byte nybbles = 0b0010_0101;"))
    (check-true (parse-class-stmt "long bytes = 0b11010010_01101001_10010100_10010010;"))
    (check-true (parse-class-stmt "long lastReceivedMessageId = 0L;"))
    (check-true (parse-class-stmt "double hexDouble1 = 0x1.0p0;"))
    (check-true (parse-class-stmt "double hexDouble2 = 0x1.956ad0aae33a4p117;"))
    (check-true (parse-class-stmt "int octal = 01234567;"))
    (check-true (parse-class-stmt "long hexUpper = 0x1234567890ABCDEFL;"))
    (check-true (parse-class-stmt "long hexLower = 0x1234567890abcedfl;")))

  ;; ---- Operator Tests ----

  (test-case "Arithmetic operators"
    (check-true (parse-class-stmt "int result = x + y;"))
    (check-true (parse-class-stmt "int result = x - y;"))
    (check-true (parse-class-stmt "int result = x * y;"))
    (check-true (parse-class-stmt "int result = y / x;"))
    (check-true (parse-class-stmt "int result = x % 3;")))

  (test-case "Unary operators"
    (check-true (parse-class-stmt "int result = +x;"))
    (check-true (parse-class-stmt "int result = -y;"))
    (check-true (parse-class-stmt "int result = ++x;"))
    (check-true (parse-class-stmt "int result = --y;"))
    (check-true (parse-class-stmt "boolean not_ok = !ok;")))

  (test-case "Prefix and postfix increment/decrement"
    (check-true (parse-class-stmt "void m() { ++x; }"))
    (check-true (parse-class-stmt "void m() { x++; }"))
    (check-true (parse-class-stmt "void m() { --y; }"))
    (check-true (parse-class-stmt "void m() { y--; }"))
    (check-true (parse-class-stmt "void m() { LexerTest.prePost++; }"))
    (check-true (parse-class-stmt "void m() { LexerTest.prePost--; }"))
    (check-true (parse-class-stmt "void m() { ++LexerTest.prePost; }"))
    (check-true (parse-class-stmt "void m() { --LexerTest.prePost; }"))
    (check-true (parse-class-stmt "void m() { this.prePost++; }"))
    (check-true (parse-class-stmt "void m() { super.prePost++; }"))
    (check-true (parse-class-stmt "void m() { ++this.prePost; }"))
    (check-true (parse-class-stmt "void m() { ++super.prePost; }"))
    (check-true (parse-class-stmt "void m() { someMethod()[0]++; }"))
    (check-true (parse-class-stmt "void m() { ++someMethod()[0]; }")))

  (test-case "Relational operators"
    (check-true (parse-class-stmt "boolean result = x == y;"))
    (check-true (parse-class-stmt "boolean result = x != y;"))
    (check-true (parse-class-stmt "boolean result = x > y;"))
    (check-true (parse-class-stmt "boolean result = x >= y;"))
    (check-true (parse-class-stmt "boolean result = x < y;"))
    (check-true (parse-class-stmt "boolean result = x <= y;")))

  (test-case "Conditional operators"
    (check-true (parse-class-stmt "void m() { if ((x > 8) && (y > 8)) {} }"))
    (check-true (parse-class-stmt "void m() { if ((x > 10) || (y > 10)) {} }"))
    (check-true (parse-class-stmt "int result = (x > 10) ? x : y;"))
    (check-true (parse-class-stmt "int f = b1 ? b2 : b3 ? 3 : 4;")))

  (test-case "Bitwise and Bit shift operators"
    (check-true (parse-class-stmt "int result = ~x;"))
    (check-true (parse-class-stmt "int result = x << 1;"))
    (check-true (parse-class-stmt "int result = x >> 2;"))
    (check-true (parse-class-stmt "int result = x >>> 3;"))
    (check-true (parse-class-stmt "int result = x & 4;"))
    (check-true (parse-class-stmt "int result = x ^ 5;"))
    (check-true (parse-class-stmt "int result = x | 6;")))

  (test-case "Assignment operators"
    (check-true (parse-class-stmt "void m() { result = x; }"))
    (check-true (parse-class-stmt "void m() { result += x; }"))
    (check-true (parse-class-stmt "void m() { result -= x; }"))
    (check-true (parse-class-stmt "void m() { result *= x; }"))
    (check-true (parse-class-stmt "void m() { result /= x; }"))
    (check-true (parse-class-stmt "void m() { result %= x; }"))
    (check-true (parse-class-stmt "void m() { result &= x; }"))
    (check-true (parse-class-stmt "void m() { result ^= x; }"))
    (check-true (parse-class-stmt "void m() { result |= x; }"))
    (check-true (parse-class-stmt "void m() { result <<= x; }"))
    (check-true (parse-class-stmt "void m() { result >>= x; }"))
    (check-true (parse-class-stmt "void m() { result >>>= x; }")))

  ;; ---- Control Flow Tests ----

  (test-case "If statements"
    (check-true (parse-class-stmt "void m() { if (i == 3) doSomething(); }"))
    (check-true (parse-class-stmt "void m() { if (i == 2) { doSomething(); } else { doSomethingElse(); } }"))
    (check-true (parse-class-stmt "void m() { if (i == 3) { doSomething(); } else if (i == 2) { doSomethingElse(); } else { doSomethingDifferent(); } }")))

  (test-case "Switch statements"
    (check-true (parse-class-stmt "void m() { switch (ch) { case 'A': doSomething(); break; case 'B': case 'C': doSomethingElse(); break; default: doSomethingDifferent(); break; } }")))

  (test-case "While and do-while statements"
    (check-true (parse-class-stmt "void m() { while (i < 10) { doSomething(); } }"))
    (check-true (parse-class-stmt "void m() { do { doSomething(); } while (i < 10); }")))

  (test-case "For statements"
    (check-true (parse-class-stmt "void m() { for (int i = 0; i < 10; i++) { doSomething(); } }"))
    (check-true (parse-class-stmt "void m() { for (int i = 0, j = 9; i < 10; i++, j -= 3) { doSomething(); } }"))
    (check-true (parse-class-stmt "void m() { for (;;) { doSomething(); } }"))
    (check-true (parse-class-stmt "void m() { for (int i : intArray) { doSomething(i); } }")))

  (test-case "Break and continue with labels"
    (check-true (parse-class-stmt "void m() { break; }"))
    (check-true (parse-class-stmt "void m() { break outer; }"))
    (check-true (parse-class-stmt "void m() { continue; }"))
    (check-true (parse-class-stmt "void m() { continue outer; }")))

  (test-case "Return statements"
    (check-true (parse-class-stmt "void m() { return; }"))
    (check-true (parse-class-stmt "int m() { return result; }")))

  (test-case "Exception handling"
    (check-true (parse-class-stmt "void m() { try { methodThrowingExceptions(); } catch (Exception ex) { reportException(ex); } finally { freeResources(); } }"))
    (check-true (parse-class-stmt "void m() { try { methodThrowingExceptions(); } catch (IOException | IllegalArgumentException ex) { reportException(ex); } }"))
    (check-true (parse-class-stmt "void m() { throw new NullPointerException(); }")))

  (test-case "Synchronized statement"
    (check-true (parse-class-stmt "void m() { synchronized (someObject) { } }")))

  (test-case "Assert statement"
    (check-true (parse-class-stmt "void m() { assert n != 0; }"))
    (check-true (parse-class-stmt "void m() { assert n != 0 : \"n was equal to zero\"; }")))

  ;; ---- Class and Interface Tests ----

  (test-case "Class declarations"
    (check-true (parse-successfully "class Foo { }"))
    (check-true (parse-successfully "public class Foo { }"))
    (check-true (parse-successfully "class Foo { class Bar { } }"))
    (check-true (parse-successfully "class Foo { static class Bar { } }")))

  (test-case "Interface declarations"
    (check-true (parse-successfully "interface ActionListener { void actionSelected(int action); }"))
    (check-true (parse-successfully "interface RequestListener { int requestReceived(); }"))
    (check-true (parse-successfully "class ActionHandler implements ActionListener, RequestListener { }")))

  (test-case "Enum declarations"
    (check-true (parse-successfully "enum Season { WINTER, SPRING, SUMMER, AUTUMN }"))
    (check-true (parse-successfully "public enum Season { WINTER, SPRING, SUMMER, AUTUMN; }")))

  ;; ---- Method Tests ----

  (test-case "Method declarations"
    (check-true (parse-class-stmt "int bar(int a, int b) { return (a*2) + b; }"))
    (check-true (parse-class-stmt "int bar(int a) { return a*2; }"))
    (check-true (parse-class-stmt "void openStream() throws IOException, myException { }"))
    (check-true (parse-class-stmt "void printReport(String header, int... numbers) { }")))

  (test-case "Constructor declarations"
    (check-true (parse-successfully "class Foo { Foo() { } }"))
    (check-true (parse-successfully "class Foo { Foo(String str) { } }"))
    (check-true (parse-successfully "class Foo { public Foo() { } }")))

  (test-case "Static and instance initializers"
    (check-true (parse-successfully "class Foo { static { } }"))
    (check-true (parse-successfully "class Foo { { } }")))

  ;; ---- Annotation Tests ----

  (test-case "Annotation declarations"
    (check-true (parse-successfully "@interface BlockingOperations { }"))
    (check-true (parse-successfully "@interface BlockingOperations { boolean fileSystemOperations(); boolean networkOperations() default false; }")))

  ;; ---- Generics Tests ----

  (test-case "Generic class"
    (check-true (parse-successfully "class Mapper<T, V> { }"))
    (check-true (parse-successfully "class Mapper<T extends ArrayList, V> { }")))

  ;; ---- Array Tests ----

  (test-case "Array declarations and access"
    (check-true (parse-class-stmt "int[] numbers = new int[5];"))
    (check-true (parse-class-stmt "void m() { numbers[0] = 2; }"))
    (check-true (parse-class-stmt "void m() { int x = numbers[0]; }")))

  ;; ---- Import and Package Tests ----

  (test-case "Package declaration"
    (check-true (parse-successfully "package myapplication.mylibrary; class Foo { }")))

  (test-case "Import declarations"
    (check-true (parse-successfully "import java.util.Random; class Foo { }"))
    (check-true (parse-successfully "import static java.lang.System.out; class Foo { }")))

  ;; ---- Instanceof Tests ----

  (test-case "Instanceof"
    (check-true (parse-class-stmt "void m() { if(args instanceof String[]) { } }")))

  ;; ---- Full File Parse Test ----

  (test-case "Parse AllInOne7.java file"
    (define java-file-path (build-path (current-directory) "example" "AllInOne7.java"))
    (when (file-exists? java-file-path)
      (define file-content (file->string java-file-path))
      (define result (parse-result file-content))
      (when (eq? (car result) 'error)
        (displayln (format "Parse error: ~a" (cdr result))))
      (check-equal? (car result) 'success
                    (format "AllInOne7.java should parse successfully. Error: ~a"
                            (if (eq? (car result) 'error) (cdr result) "none")))))

  ;; Display test results summary
  (displayln "=== Java Parser Tests Complete ==="))

;; Run
(main)

;; ----------------------------------------------------------------------------
;; Exports
;; ----------------------------------------------------------------------------

(provide (all-defined-out))
