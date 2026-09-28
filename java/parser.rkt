#lang racket/base
(require parser-tools/lex
         (only-in parser-tools/private-lex/token make-token)
         "lexer.rkt"
         "ext-parser.rkt"
         "ast.rkt")

(provide java-parser
         parse-java-code
         parse-java-file
         (all-from-out "ast.rkt"))

(define (skip-token? tok)
  (define token-value (position-token-token tok))
  (and (token? token-value)
       (case (token-name token-value)
         [(WS COMMENT LINE_COMMENT) #t]
         [else #f])))

(define (next-significant-token port)
  (let loop ()
    (define tok (java-lexer port))
    (if (skip-token? tok)
        (loop)
        tok)))

(struct token-context (port
                       [pending #:mutable]
                       [previous #:mutable]
                       [last #:mutable]))

(define contextual-keyword-token-values
  '([MODULE . "module"]
    [OPEN . "open"]
    [REQUIRES . "requires"]
    [EXPORTS . "exports"]
    [OPENS . "opens"]
    [TO . "to"]
    [USES . "uses"]
    [PROVIDES . "provides"]
    [WHEN . "when"]
    [WITH . "with"]
    [TRANSITIVE . "transitive"]
    [YIELD . "yield"]
    [SEALED . "sealed"]
    [PERMITS . "permits"]
    [RECORD . "record"]
    [VAR . "var"]))

(define identifier-token-names
  '(IDENTIFIER MODULE OPEN REQUIRES EXPORTS OPENS TO USES PROVIDES
               WHEN WITH TRANSITIVE YIELD SEALED PERMITS RECORD VAR))

(define expression-start-token-names
  '(IDENTIFIER MODULE OPEN REQUIRES EXPORTS OPENS TO USES PROVIDES
               WHEN WITH TRANSITIVE YIELD SEALED PERMITS RECORD VAR
               THIS SUPER NEW LPAREN DECIMAL_LITERAL HEX_LITERAL OCT_LITERAL
               BINARY_LITERAL FLOAT_LITERAL HEX_FLOAT_LITERAL BOOL_LITERAL
               CHAR_LITERAL STRING_LITERAL TEXT_BLOCK NULL_LITERAL ADD SUB
               INC DEC TILDE BANG SWITCH))

(define cast-lookahead-blockers
  '(IF WHILE FOR SWITCH SYNCHRONIZED))

(define (context-read-token ctx)
  (define pending (token-context-pending ctx))
  (if (null? pending)
      (next-significant-token (token-context-port ctx))
      (let ([tok (car pending)])
        (set-token-context-pending! ctx (cdr pending))
        tok)))

(define (context-unread-token! ctx tok)
  (set-token-context-pending! ctx (cons tok (token-context-pending ctx))))

(define (restore-read-tokens! ctx read-tokens-rev)
  (for ([read-tok (in-list read-tokens-rev)])
    (context-unread-token! ctx read-tok)))

(define (retag-token tok name end-tok)
  (make-position-token name
                       (position-token-start-pos tok)
                       (position-token-end-pos end-tok)))

(define (retag-value-token tok name)
  (make-position-token (make-token name (token-value (position-token-token tok)))
                       (position-token-start-pos tok)
                       (position-token-end-pos tok)))

(define (retag-primitive-token tok token-name primitive-value)
  (make-position-token (make-token token-name primitive-value)
                       (position-token-start-pos tok)
                       (position-token-end-pos tok)))

(define (identifier-value-token name tok value)
  (make-position-token (make-token name value)
                       (position-token-start-pos tok)
                       (position-token-end-pos tok)))

(define (token-name-of tok)
  (token-name (position-token-token tok)))

(define (identifier-token? tok)
  (memq (token-name-of tok) identifier-token-names))

(define (expression-start-token? tok)
  (memq (token-name-of tok) expression-start-token-names))

(define (declaration-delimiter-token? tok)
  (memq (token-name-of tok) '(ASSIGN COMMA SEMI)))

(define (primitive-token-value token-value)
  (case token-value
    [(BOOLEAN) 'boolean]
    [(CHAR) 'char]
    [(BYTE) 'byte]
    [(SHORT) 'short]
    [(INT) 'int]
    [(LONG) 'long]
    [(FLOAT) 'float]
    [(DOUBLE) 'double]
    [else #f]))

(define (matching-gt-before-expression-boundary? ctx)
  (let loop ([depth 0] [read-tokens-rev '()])
    (define lookahead-tok (context-read-token ctx))
    (define lookahead-name (token-name-of lookahead-tok))
    (define next-read-tokens-rev (cons lookahead-tok read-tokens-rev))
    (cond
      [(eq? lookahead-name 'EOF)
       (restore-read-tokens! ctx next-read-tokens-rev)
       #f]
      [(and (zero? depth)
            (memq lookahead-name '(RPAREN SEMI ASSIGN COLON ARROW LBRACE RBRACE)))
       (restore-read-tokens! ctx next-read-tokens-rev)
       #f]
      [(eq? lookahead-name 'LT)
       (loop (add1 depth) next-read-tokens-rev)]
      [(and (eq? lookahead-name 'GT) (positive? depth))
       (loop (sub1 depth) next-read-tokens-rev)]
      [(eq? lookahead-name 'GT)
       (restore-read-tokens! ctx next-read-tokens-rev)
       #t]
      [(and (eq? lookahead-name 'GT_SINGLE) (positive? depth))
       (loop (sub1 depth) next-read-tokens-rev)]
      [(and (eq? lookahead-name 'GT_SINGLE) (zero? depth))
       (restore-read-tokens! ctx next-read-tokens-rev)
       #t]
      [else
       (loop depth next-read-tokens-rev)])))

(define (for-primitive-token-name ctx)
  (define maybe-name-tok (context-read-token ctx))
  (define delimiter-tok
    (cond
      [(eq? (token-name-of maybe-name-tok) 'LBRACK)
       (define maybe-rbrack-tok (context-read-token ctx))
       (define maybe-array-name-tok (context-read-token ctx))
       (define maybe-delimiter-tok (context-read-token ctx))
       (context-unread-token! ctx maybe-delimiter-tok)
       (context-unread-token! ctx maybe-array-name-tok)
       (context-unread-token! ctx maybe-rbrack-tok)
       (context-unread-token! ctx maybe-name-tok)
       maybe-delimiter-tok]
      [else
       (define maybe-delimiter-tok (context-read-token ctx))
       (context-unread-token! ctx maybe-delimiter-tok)
       (context-unread-token! ctx maybe-name-tok)
       maybe-delimiter-tok]))
  (if (eq? (token-name-of delimiter-tok) 'COLON)
      'PRIMITIVE_ENHANCED
      'PRIMITIVE_DECL))

(define (skip-balanced-after-lparen ctx read-tokens-rev)
  (let loop ([depth 1] [read-tokens-rev read-tokens-rev])
    (define tok (context-read-token ctx))
    (define name (token-name-of tok))
    (define next-read-tokens-rev (cons tok read-tokens-rev))
    (cond
      [(eq? name 'EOF) next-read-tokens-rev]
      [(eq? name 'LPAREN) (loop (add1 depth) next-read-tokens-rev)]
      [(and (eq? name 'RPAREN) (= depth 1)) next-read-tokens-rev]
      [(eq? name 'RPAREN) (loop (sub1 depth) next-read-tokens-rev)]
      [else (loop depth next-read-tokens-rev)])))

(define (read-annotation-tail ctx read-tokens-rev)
  (define name-tok (context-read-token ctx))
  (let loop ([read-tokens-rev (cons name-tok read-tokens-rev)])
    (define maybe-tail-tok (context-read-token ctx))
    (define maybe-tail-name (token-name-of maybe-tail-tok))
    (define next-read-tokens-rev (cons maybe-tail-tok read-tokens-rev))
    (cond
      [(eq? maybe-tail-name 'DOT)
       (define segment-tok (context-read-token ctx))
       (loop (cons segment-tok next-read-tokens-rev))]
      [(eq? maybe-tail-name 'LPAREN)
       (skip-balanced-after-lparen ctx next-read-tokens-rev)]
      [else
       (context-unread-token! ctx maybe-tail-tok)
       read-tokens-rev])))

(define (varargs-annotation-lookahead? ctx)
  (let loop ([read-tokens-rev '()])
    (define after-annotation-rev (read-annotation-tail ctx read-tokens-rev))
    (define next-tok (context-read-token ctx))
    (define next-name (token-name-of next-tok))
    (define next-read-tokens-rev (cons next-tok after-annotation-rev))
    (cond
      [(eq? next-name 'ELLIPSIS)
       (restore-read-tokens! ctx next-read-tokens-rev)
       #t]
      [(eq? next-name 'AT)
       (loop next-read-tokens-rev)]
      [else
       (restore-read-tokens! ctx next-read-tokens-rev)
       #f])))

(define (consume-through-ellipsis! ctx)
  (let loop ()
    (define tok (context-read-token ctx))
    (if (or (eq? (token-name-of tok) 'ELLIPSIS)
            (eq? (token-name-of tok) 'EOF))
        tok
        (loop))))

(define (skip-current-annotation-before-type ctx)
  (define annotation-tokens-rev (read-annotation-tail ctx '()))
  (define next-tok (context-read-token ctx))
  (cond
    [(identifier-token? next-tok)
     (context-unread-token! ctx next-tok)
     (normalize-token ctx (context-read-token ctx))]
    [else
     (context-unread-token! ctx next-tok)
     (restore-read-tokens! ctx annotation-tokens-rev)
     #f]))

(define (normalize-for-primitive-token ctx tok token-value)
  (and (eq? (token-context-previous ctx) 'FOR)
       (eq? (token-context-last ctx) 'LPAREN)
       (primitive-token-value token-value)
       (retag-primitive-token tok
                              (for-primitive-token-name ctx)
                              (primitive-token-value token-value))))

(define (normalize-receiver-this ctx tok token-value)
  (and (eq? token-value 'THIS)
       (memq (token-context-last ctx) '(IDENTIFIER TYPE_IDENTIFIER))
       (let ([next-tok (context-read-token ctx)])
         (if (memq (token-name-of next-tok) '(RPAREN COMMA))
             (begin
               (context-unread-token! ctx next-tok)
               (identifier-value-token 'IDENTIFIER tok "this"))
             (begin
               (context-unread-token! ctx next-tok)
               #f)))))

(define (normalize-annotation-token ctx tok)
  (define last-token-name (token-context-last ctx))
  (define previous-token-name (token-context-previous ctx))
  (cond
    [(eq? last-token-name 'INSTANCEOF)
     (retag-token tok 'PATTERN_AT tok)]
    [(and (eq? last-token-name 'LPAREN)
          (not (memq previous-token-name '(IDENTIFIER TYPE_IDENTIFIER THIS SUPER))))
     (retag-token tok 'CAST_AT tok)]
    [(and (memq last-token-name
                '(IDENTIFIER TYPE_IDENTIFIER INSTANCEOF_PATTERN_TYPE
                             BOOLEAN BYTE CHAR SHORT INT LONG FLOAT DOUBLE))
          (varargs-annotation-lookahead? ctx))
     (retag-token tok 'EMPTY_BRACKETS (consume-through-ellipsis! ctx))]
    [(and (eq? previous-token-name 'AT)
          (identifier-token? (make-position-token (make-token last-token-name #f)
                                                  (position-token-start-pos tok)
                                                  (position-token-end-pos tok))))
     (or (skip-current-annotation-before-type ctx) tok)]
    [else
     (define next-tok (context-read-token ctx))
     (if (eq? (token-name-of next-tok) 'INTERFACE)
         (retag-token tok 'AT_INTERFACE next-tok)
         (begin
           (context-unread-token! ctx next-tok)
           tok))]))

(define (normalize-dot-token ctx tok)
  (define next-tok (context-read-token ctx))
  (cond
    [(eq? (token-name-of next-tok) 'MUL)
     (retag-token tok 'DOT_MUL next-tok)]
    [(eq? (token-name-of next-tok) 'AT)
     (read-annotation-tail ctx '())
     tok]
    [else
     (context-unread-token! ctx next-tok)
     tok]))

(define (normalize-empty-brackets-token ctx tok)
  (define next-tok (context-read-token ctx))
  (if (eq? (token-name-of next-tok) 'RBRACK)
      (retag-token tok 'EMPTY_BRACKETS next-tok)
      (begin
        (context-unread-token! ctx next-tok)
        tok)))

(define (normalize-lparen-token ctx tok)
  (define maybe-type-tok (context-read-token ctx))
  (define maybe-rparen-tok (context-read-token ctx))
  (define maybe-expr-tok (context-read-token ctx))
  (define cast-start?
    (and (not (memq (token-context-last ctx) cast-lookahead-blockers))
         (eq? (token-name-of maybe-type-tok) 'IDENTIFIER)))
  (cond
    [(and cast-start?
          (eq? (token-name-of maybe-rparen-tok) 'RPAREN)
          (expression-start-token? maybe-expr-tok))
     (context-unread-token! ctx maybe-expr-tok)
     (context-unread-token! ctx maybe-rparen-tok)
     (context-unread-token! ctx (retag-value-token maybe-type-tok 'TYPE_IDENTIFIER))
     tok]
    [(and cast-start?
          (eq? (token-name-of maybe-rparen-tok) 'BITAND))
     (context-unread-token! ctx maybe-expr-tok)
     (context-unread-token! ctx maybe-rparen-tok)
     (context-unread-token! ctx (retag-value-token maybe-type-tok 'TYPE_IDENTIFIER))
     tok]
    [else
     (context-unread-token! ctx maybe-expr-tok)
     (context-unread-token! ctx maybe-rparen-tok)
     (context-unread-token! ctx maybe-type-tok)
     tok]))

(define (identifier-array-type-start? ctx)
  (define maybe-rbrack-tok (context-read-token ctx))
  (cond
    [(eq? (token-name-of maybe-rbrack-tok) 'RBRACK)
     (define maybe-name-tok (context-read-token ctx))
     (context-unread-token! ctx maybe-name-tok)
     (context-unread-token! ctx maybe-rbrack-tok)
     (identifier-token? maybe-name-tok)]
    [else
     (context-unread-token! ctx maybe-rbrack-tok)
     #f]))

(define (identifier-qualified-type-start? ctx)
  (define segment-tok (context-read-token ctx))
  (define maybe-name-tok (context-read-token ctx))
  (define maybe-delimiter-tok (context-read-token ctx))
  (context-unread-token! ctx maybe-delimiter-tok)
  (context-unread-token! ctx maybe-name-tok)
  (context-unread-token! ctx segment-tok)
  (and (identifier-token? segment-tok)
       (or (and (identifier-token? maybe-name-tok)
                (declaration-delimiter-token? maybe-delimiter-tok))
           (and (eq? (token-name-of maybe-name-tok) 'LT)
                (not (eq? (token-context-last ctx) 'NEW))))))

(define (normalize-identifier-token ctx tok)
  (define last-token-name (token-context-last ctx))
  (cond
    [(eq? last-token-name 'CASE)
     (define maybe-name-tok (context-read-token ctx))
     (context-unread-token! ctx maybe-name-tok)
     (if (identifier-token? maybe-name-tok)
         (retag-value-token tok 'INSTANCEOF_PATTERN_TYPE)
         tok)]
    [(eq? last-token-name 'INSTANCEOF)
     (define maybe-name-tok (context-read-token ctx))
     (context-unread-token! ctx maybe-name-tok)
     (if (identifier-token? maybe-name-tok)
         (retag-value-token tok 'INSTANCEOF_PATTERN_TYPE)
         tok)]
    [else
     (define maybe-lbrack-tok (context-read-token ctx))
     (cond
       [(and (not (eq? last-token-name 'AT))
             (eq? (token-name-of maybe-lbrack-tok) 'LBRACK))
        (define array-type-start? (identifier-array-type-start? ctx))
        (context-unread-token! ctx maybe-lbrack-tok)
        (if array-type-start?
            (retag-value-token tok 'TYPE_IDENTIFIER)
            tok)]
       [(and (not (eq? last-token-name 'AT))
             (eq? (token-name-of maybe-lbrack-tok) 'DOT))
        (define qualified-type-start? (identifier-qualified-type-start? ctx))
        (context-unread-token! ctx maybe-lbrack-tok)
        (if qualified-type-start?
            (retag-value-token tok 'TYPE_IDENTIFIER)
            tok)]
       [else
        (context-unread-token! ctx maybe-lbrack-tok)
        tok])]))

(define (normalize-lt-token ctx tok)
  (define next-tok (context-read-token ctx))
  (cond
    [(eq? (token-name-of next-tok) 'LT)
     (context-unread-token! ctx next-tok)
     tok]
    [else
     (context-unread-token! ctx next-tok)
     (if (matching-gt-before-expression-boundary? ctx)
         tok
         (retag-token tok 'LT_SINGLE tok))]))

(define (normalize-gt-token ctx tok)
  (define next-tok (context-read-token ctx))
  (cond
    [(eq? (token-name-of next-tok) 'GT)
     (context-unread-token! ctx next-tok)
     tok]
    [else
     (context-unread-token! ctx next-tok)
     (retag-token tok 'GT_SINGLE tok)]))

(define (normalize-var-token ctx tok)
  (define next-tok (context-read-token ctx))
  (cond
    [(eq? (token-name-of next-tok) 'DOT)
     (context-unread-token! ctx next-tok)
     (identifier-value-token 'IDENTIFIER tok "var")]
    [(identifier-token? next-tok)
     (define next-next-tok (context-read-token ctx))
     (define next-next-token-value (token-name-of next-next-tok))
     (context-unread-token! ctx next-next-tok)
     (context-unread-token! ctx next-tok)
     (if (memq next-next-token-value '(ASSIGN COLON COMMA RPAREN))
         (retag-token tok 'VAR_DECL tok)
         tok)]
    [else
     (context-unread-token! ctx next-tok)
     tok]))

(define (normalize-contextual-keyword-token ctx tok token-value)
  (define token-name/value (assoc token-value contextual-keyword-token-values))
  (and token-name/value
       (let ([next-tok (context-read-token ctx)])
         (if (memq (token-name-of next-tok) '(DOT LT))
             (begin
               (context-unread-token! ctx next-tok)
               (identifier-value-token 'IDENTIFIER tok (cdr token-name/value)))
             (begin
               (context-unread-token! ctx next-tok)
               #f)))))

(define (normalize-token ctx tok)
  (define token-value (token-name-of tok))
  (or (normalize-for-primitive-token ctx tok token-value)
      (normalize-receiver-this ctx tok token-value)
      (case token-value
        [(AT) (normalize-annotation-token ctx tok)]
        [(DOT) (normalize-dot-token ctx tok)]
        [(LBRACK) (normalize-empty-brackets-token ctx tok)]
        [(LPAREN) (normalize-lparen-token ctx tok)]
        [(IDENTIFIER) (normalize-identifier-token ctx tok)]
        [(LT) (normalize-lt-token ctx tok)]
        [(GT) (normalize-gt-token ctx tok)]
        [(VAR) (normalize-var-token ctx tok)]
        [else (normalize-contextual-keyword-token ctx tok token-value)])
      tok))

(define (make-next-significant-token port)
  (define ctx (token-context port '() #f #f))
  (lambda ()
    (define tok (normalize-token ctx (context-read-token ctx)))
    (set-token-context-previous! ctx (token-context-last ctx))
    (set-token-context-last! ctx (token-name-of tok))
    tok))

(define (fold-left-expression first rest)
  (for/fold ([acc first])
            ([op/rhs (in-list rest)])
    (ast-expression (list acc (car op/rhs) (cadr op/rhs)))))

(define (apply-postfix-suffixes base suffixes postfix-op)
  (define with-suffixes
    (for/fold ([acc base])
              ([suffix (in-list suffixes)])
      (ast-expression (cons acc suffix))))
  (if (null? postfix-op)
      with-suffixes
      (ast-expression (list with-suffixes postfix-op))))

(define (make-variable-declarator-id name array-suffixes)
  (ast-variable-declarator-id (list name array-suffixes)))

(define (make-empty-bracket-array-suffixes)
  (list (ast-type-array-suffix (list '() 'LBRACK 'RBRACK))))

(define (make-named-formal-parameter modifiers type varargs name array-suffixes)
  (ast-formal-parameter
   (list modifiers type varargs (make-variable-declarator-id name array-suffixes))))

(define (make-type-identifier-pattern type name)
  (ast-case-pattern
   (list
    (ast-pattern
     (list '()
           type
           '()
           (list
            (ast-variable-declarator
             (list (make-variable-declarator-id name '()) '()))))))))

(define (make-reference-type name type-arguments suffixes array-suffixes)
  (ast-type-type
   (list '()
         (ast-type-base
          (list
           (ast-class-or-interface-type
            (list (ast-class-type (list name type-arguments suffixes))))))
         array-suffixes)))

(define (make-annotated-reference-type annotations name type-arguments suffixes array-suffixes)
  (ast-type-type
   (list annotations
         (ast-type-base
          (list
           (ast-class-or-interface-type
            (list (ast-class-type (list name type-arguments suffixes))))))
         array-suffixes)))

(define (make-primitive-type primitive array-suffixes)
  (ast-type-type (list '() (ast-type-base (list primitive)) array-suffixes)))

(define (make-annotated-primitive-type annotations primitive array-suffixes)
  (ast-type-type (list annotations (ast-type-base (list primitive)) array-suffixes)))

(define (make-local-variable-block modifiers rest)
  (ast-block-statement
   (list (ast-local-variable-declaration (list modifiers rest)))))

(define (make-local-type-rest type declarators)
  (ast-local-variable-declaration-rest (list type declarators)))

(define (make-local-type-block modifiers type declarators)
  (make-local-variable-block modifiers (make-local-type-rest type declarators)))

(define (make-primitive-local-type-rest primitive array-suffixes declarators)
  (make-local-type-rest (make-primitive-type primitive array-suffixes) declarators))

(define (make-reference-local-type-block name type-arguments suffixes array-suffixes declarators)
  (make-local-type-block '()
                         (make-reference-type name type-arguments suffixes array-suffixes)
                         declarators))

(define (make-for-local-type-control type declarators condition update)
  (ast-for-control
   (list
    (ast-for-init
     (list (ast-local-variable-declaration
            (list '() (make-local-type-rest type declarators)))))
    'SEMI
    condition
    'SEMI
    update)))

(define (make-for-reference-local-type-control
         name type-arguments suffixes array-suffixes declarators condition update)
  (make-for-local-type-control
   (make-reference-type name type-arguments suffixes array-suffixes)
   declarators
   condition
   update))

(define (make-enhanced-for-type-control modifiers type declarator-id expr)
  (ast-for-control
   (list (ast-enhanced-for-control
          (list modifiers type declarator-id 'COLON expr)))))

(define (make-reference-enhanced-for-control
         modifiers name type-arguments suffixes array-suffixes declarator-id expr)
  (make-enhanced-for-type-control
   modifiers
   (make-reference-type name type-arguments suffixes array-suffixes)
   declarator-id
   expr))

(define (make-var-type-rest name expr)
  (ast-local-variable-declaration-rest (list 'VAR name 'ASSIGN expr)))

(define (make-interface-common-method type name parameters brackets throws body)
  (ast-interface-common-body-declaration
   (list '()
         (ast-type-type-or-void (list type))
         name
         parameters
         brackets
         throws
         body)))

(define (make-interface-member-method type name parameters brackets throws body)
  (ast-interface-member-declaration
   (list
    (ast-interface-method-declaration
     (list '() (make-interface-common-method type name parameters brackets throws body))))))

(define (make-simple-method-reference name type-arguments method-name)
  (ast-expression
   (list (ast-expression (list (ast-primary (list name))))
         'COLONCOLON
         type-arguments
         method-name)))

(define (make-identifier-expression name)
  (ast-expression (list (ast-primary (list name)))))

(define-grammar-operator (? s)
  [main
   [() '()]
   [(s) $1]])
(define-grammar-operator (?->bool s)
  [main
   [() #f]
   [(s) #t]])
(define-grammar-operator (* s)
  [main
   [() '()]
   [(s main) (cons $1 $2)]])
(define-grammar-operator (*-prec s precedence)
  [main
   [() (prec precedence) '()]
   [(s main) (cons $1 $2)]])
(define-grammar-operator (+ s)
  [main
   [(s) (cons $1 '())]
   [(s main) (cons $1 $2)]])
(define-grammar-operator (sep-by separator element)
  [main
   [(element (* rest)) (cons $1 $2)]]
  [rest
   [(separator element) $2]])

(define java-parser
  (ext-parser
   [start compilationUnit]
   [end EOF]
   [error (lambda (tok-ok? tok-name tok-value start end)
            (error 'java-parser "Parse error at line ~a, col ~a: ~a ~a"
                   (position-line start) (position-col start)
                   tok-name tok-value))]
   [src-pos]
   [tokens empty-tokens tokens]
   [precs (right ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN
                 AND_ASSIGN OR_ASSIGN XOR_ASSIGN MOD_ASSIGN
                 LSHIFT_ASSIGN RSHIFT_ASSIGN URSHIFT_ASSIGN)
          (right QUESTION)
          (left OR)
          (left AND)
          (left BITOR)
          (left CARET)
          (left BITAND)
          (left EQUAL NOTEQUAL)
          (left LT_SINGLE GT_SINGLE LE GE INSTANCEOF)
          (left LT GT)
          (left ADD SUB)
          (left MUL DIV MOD)
          (right AT FINAL LBRACK EMPTY_BRACKETS)]
   [expected-SR-conflicts 1631]
   [expected-RR-conflicts 968]
   [grammar
    [compilationUnit
     [((* annotation) compilationUnit.1) (ast-compilation-unit (list $1 $2))]]
    [compilationUnit.1
     [(PACKAGE qualifiedName SEMI compilationUnit.2) (ast-compilation-unit-body (list 'PACKAGE $2 'SEMI $4))]
     [(compilationUnit.2) (ast-compilation-unit-body (list $1))]]
    [compilationUnit.2
     [() '()]
     [(compilationUnit.2 compilationUnit.3) (append $1 (list $2))]]
    [compilationUnit.3
     [(importDeclaration) (ast-compilation-unit-import (list $1))]
     [(typeDeclaration) (ast-compilation-unit-type (list $1))]
     [(moduleDeclaration) (ast-modular-compilation-unit (list '() $1))]
     [(SEMI) '()]]
    [modularCompulationUnit
     [((* importDeclaration) moduleDeclaration) (ast-modular-compilation-unit (list $1 $2))]]
    [packageDeclaration
     [((* annotation) PACKAGE qualifiedName SEMI) (ast-package-declaration (list $1 'PACKAGE $3 'SEMI))]]
    [importDeclaration
     [(IMPORT (?->bool STATIC) identifier importNameTail) (ast-import-declaration (list 'IMPORT $2 (cons $3 (car $4)) (cadr $4) 'SEMI))]]
    [importNameTail
     [(SEMI) (list '() #f)]
     [(DOT_MUL SEMI) (list '() #t)]
     [(DOT identifier importNameTail) (list (cons $2 (car $3)) (cadr $3))]]
    [typeDeclaration
     [(typeDeclaration.2) (ast-type-declaration (list '() $1))]
     [(classOrInterfaceModifier typeDeclaration) (ast-type-declaration (list (cons $1 (car (ast-type-declaration-children $2)))
                                                                              (cadr (ast-type-declaration-children $2))))]]
    [typeDeclarationModifier
     [(PUBLIC) 'public]
     [(PROTECTED) 'protected]
     [(PRIVATE) 'private]
     [(STATIC) 'static]
     [(ABSTRACT) 'abstract]
     [(FINAL) 'final]
     [(STRICTFP) 'strictfp]
     [(SEALED) 'sealed]
     [(NON_SEALED) 'non-sealed]]
    [typeDeclaration.2
     [(classDeclaration) (ast-type-declaration-body (list $1))]
     [(enumDeclaration) (ast-type-declaration-body (list $1))]
     [(interfaceDeclaration) (ast-type-declaration-body (list $1))]
     [(annotationTypeDeclaration) (ast-type-declaration-body (list $1))]
     [(recordDeclaration) (ast-type-declaration-body (list $1))]]
    [modifier
     [(classOrInterfaceModifier) (ast-modifier (list $1))]
     [(NATIVE) 'native]
     [(SYNCHRONIZED) 'synchronized]
     [(TRANSIENT) 'transient]
     [(VOLATILE) 'volatile]]
    [classOrInterfaceModifier
     [(annotation) (ast-class-or-interface-modifier (list $1))]
     [(PUBLIC) 'public]
     [(PROTECTED) 'protected]
     [(PRIVATE) 'private]
     [(STATIC) 'static]
     [(ABSTRACT) 'abstract]
     [(FINAL) 'final]
     [(STRICTFP) 'strictfp]
     [(SEALED) 'sealed]
     [(NON_SEALED) 'non-sealed]]
    [variableModifier
     [(FINAL) 'final]
     [(annotation) (ast-variable-modifier (list $1))]]
    [classDeclaration
     [(CLASS identifier (? typeParameters) (? classDeclaration.4) (? classDeclaration.5) (? classDeclaration.6) classBody) (ast-class-declaration (list 'CLASS $2 $3 $4 $5 $6 $7))]]
    [classDeclaration.4
     [(EXTENDS typeType) (ast-class-declaration-extends (list 'EXTENDS $2))]]
    [classDeclaration.5
     [(IMPLEMENTS typeList) (ast-class-declaration-implements (list 'IMPLEMENTS $2))]]
    [classDeclaration.6
     [(PERMITS typeList) (ast-class-declaration-permits (list 'PERMITS $2))]]
    [typeParameters
     [(lt (sep-by COMMA typeParameter) gt) $2]]
    [typeParameter
     [((* annotation) identifier (? typeParameter.3)) (ast-type-parameter (list $1 $2 $3))]]
    [typeParameter.3
     [(EXTENDS (* annotation) typeBound) (ast-type-parameter-bound (list 'EXTENDS $2 $3))]]
    [typeBound
     [((sep-by BITAND typeType)) $1]]
    [enumDeclaration
     [(ENUM identifier (? enumDeclaration.3) LBRACE (? enumConstants) (?->bool COMMA) (? enumBodyDeclarations) RBRACE) (ast-enum-declaration (list 'ENUM $2 $3 'LBRACE $5 $6 $7 'RBRACE))]]
    [enumConstants
     [((sep-by COMMA enumConstant)) (ast-enum-constants (list $1))]]
    [enumConstant
     [((* annotation) identifier (? arguments) (? classBody)) (ast-enum-constant (list $1 $2 $3 $4))]]
    [enumBodyDeclarations
     [(SEMI (* classBodyDeclaration)) (ast-enum-body-declarations (list 'SEMI $2))]]
    [interfaceDeclaration
     [(INTERFACE identifier (? typeParameters) (? interfaceDeclaration.4) (? interfaceDeclaration.5) interfaceBody) (ast-interface-declaration (list 'INTERFACE $2 $3 $4 $5 $6))]]
    [interfaceDeclaration.4
     [(EXTENDS typeList) (ast-interface-declaration-extends (list 'EXTENDS $2))]]
    [interfaceDeclaration.5
     [(PERMITS typeList) (ast-interface-declaration-permits (list 'PERMITS $2))]]
    [classBody
     [(LBRACE (* classBodyDeclaration) RBRACE) (ast-class-body (list 'LBRACE $2 'RBRACE))]]
    [interfaceBody
     [(LBRACE (* interfaceBodyDeclaration) RBRACE) (ast-interface-body (list 'LBRACE $2 'RBRACE))]]
    [classBodyDeclaration
     [(SEMI) (ast-class-body-declaration (list 'SEMI))]
     [((?->bool STATIC) block) (ast-class-body-declaration (list $1 $2))]
     [((* modifier) memberDeclaration) (ast-class-body-declaration (list $1 $2))]]
    [memberDeclaration
     [(recordDeclaration) (ast-member-declaration (list $1))]
     [(memberCommonDeclaration) (ast-member-declaration (list $1))]
     [(typeParameters memberCommonDeclaration) (ast-generic-member-declaration (list $1 $2))]
     [(interfaceDeclaration) (ast-member-declaration (list $1))]
     [(annotationTypeDeclaration) (ast-member-declaration (list $1))]
     [(classDeclaration) (ast-member-declaration (list $1))]
     [(enumDeclaration) (ast-member-declaration (list $1))]]
    [memberCommonDeclaration
     [(typeTypeOrVoid memberCommonDeclaration.2) (ast-member-common-declaration (list $1 $2))]]
    [memberCommonDeclaration.2
     [(identifier formalParameters (* brackets) (? methodDeclaration.5) methodBody)
      (ast-method-declaration (list $1 $2 $3 $4 $5))]
     [(formalParameters (? constructorDeclaration.3) block)
      (ast-constructor-declaration (list $1 $2 $3))]
     [(identifier (* brackets) (? variableDeclarator.2) (* memberCommonDeclaration.3) SEMI)
      (ast-field-declaration (list
            (cons (ast-variable-declarator (list
                        (ast-variable-declarator-id (list $1 $2))
                        $3))
                  $4)
            'SEMI))]]
    [memberCommonDeclaration.3
     [(COMMA variableDeclarator) $2]]
    [methodDeclaration
     [(typeTypeOrVoid identifier formalParameters (* brackets) (? methodDeclaration.5) methodBody) (ast-method-declaration (list $1 $2 $3 $4 $5 $6))]]
    [brackets
     [(emptyBrackets) (ast-brackets (list 'LBRACK 'RBRACK))]]
    [methodDeclaration.5
     [(THROWS qualifiedNameList) (ast-method-declaration-throws (list 'THROWS $2))]]
    [methodBody
     [(block) (ast-method-body (list $1))]
     [(SEMI) (ast-method-body (list 'SEMI))]]
    [typeTypeOrVoid
     [(typeType) (ast-type-type-or-void (list $1))]
     [(VOID) (ast-type-type-or-void (list 'VOID))]]
    [genericMethodDeclaration
     [(typeParameters methodDeclaration) (ast-generic-method-declaration (list $1 $2))]]
    [genericConstructorDeclaration
     [(typeParameters constructorDeclaration) (ast-generic-constructor-declaration (list $1 $2))]]
    [constructorDeclaration
     [(identifier formalParameters (? constructorDeclaration.3) block) (ast-constructor-declaration (list $1 $2 $3 $4))]] ;constructorBody = block
    [constructorDeclaration.3
     [(THROWS qualifiedNameList) (ast-constructor-declaration-throws (list 'THROWS $2))]]
    [compactConstructorDeclaration
     [((* modifier) identifier block) (ast-compact-constructor-declaration (list $1 $2 $3))]] ;constructorBody = block
    [fieldDeclaration
     [(typeType variableDeclarators SEMI) (ast-field-declaration (list $1 $2 'SEMI))]]
    [interfaceBodyDeclaration
     [((* modifier) interfaceMemberDeclaration) (ast-interface-body-declaration (list $1 $2))]
     [(SEMI) (ast-interface-body-declaration (list 'SEMI))]]
    [interfaceMemberDeclaration
     [(recordDeclaration) (ast-interface-member-declaration (list $1))]
     [(primitiveType (*-prec typeType.3 EMPTY_BRACKETS) identifier (* brackets) ASSIGN variableInitializer (* interfaceCommonMemberDeclaration.3) SEMI)
      (ast-interface-member-declaration
       (list
        (ast-const-declaration
         (list
          (cons (ast-constant-declarator (list $3 $4 'ASSIGN $6)) $7)
          'SEMI))))]
     [(primitiveType (*-prec typeType.3 EMPTY_BRACKETS) identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody)
      (make-interface-member-method (make-primitive-type $1 $2) $3 $4 $5 $6 $7)]
     [(IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody)
      (make-interface-member-method (make-reference-type $1 $2 $3 $4) $5 $6 $7 $8 $9)]
     [(interfaceMethodDeclaration) (ast-interface-member-declaration (list $1))]
     [(genericInterfaceMethodDeclaration) (ast-interface-member-declaration (list $1))]
     [(constDeclaration) (ast-interface-member-declaration (list $1))]
     [(interfaceCommonMemberDeclaration) (ast-interface-member-declaration (list $1))]
     [(typeParameters interfaceCommonMemberDeclaration) (ast-generic-interface-member-declaration (list $1 $2))]
     [(interfaceDeclaration) (ast-interface-member-declaration (list $1))]
     [(annotationTypeDeclaration) (ast-interface-member-declaration (list $1))]
     [(classDeclaration) (ast-interface-member-declaration (list $1))]
     [(enumDeclaration) (ast-interface-member-declaration (list $1))]]
    [interfaceCommonMemberDeclaration
     [(typeTypeOrVoid interfaceCommonMemberDeclaration.2) (ast-interface-common-member-declaration (list $1 $2))]]
    [interfaceCommonMemberDeclaration.2
     [(identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody)
      (ast-interface-common-body-declaration (list '() $1 $2 $3 $4 $5))]
     [(identifier (* brackets) ASSIGN variableInitializer (* interfaceCommonMemberDeclaration.3) SEMI)
      (ast-const-declaration (list
            (cons (ast-constant-declarator (list $1 $2 'ASSIGN $4)) $5)
            'SEMI))]]
    [interfaceCommonMemberDeclaration.3
     [(COMMA constantDeclarator) $2]]
    [constDeclaration
     [(typeType (sep-by COMMA constantDeclarator) SEMI) (ast-const-declaration (list $1 $2 'SEMI))]]
    [constantDeclarator
     [(identifier (* brackets) ASSIGN variableInitializer) (ast-constant-declarator (list $1 $2 'ASSIGN $4))]]
    [interfaceMethodDeclaration
     [((* interfaceMethodModifier) interfaceCommonBodyDeclaration) (ast-interface-method-declaration (list $1 $2))]]
    [interfaceMethodModifier
     [(annotation) (ast-interface-method-modifier (list $1))]
     [(PUBLIC) (ast-interface-method-modifier (list 'PUBLIC))]
     [(ABSTRACT) (ast-interface-method-modifier (list 'ABSTRACT))]
     [(DEFAULT) (ast-interface-method-modifier (list 'DEFAULT))]
     [(STATIC) (ast-interface-method-modifier (list 'STATIC))]
     [(STRICTFP) (ast-interface-method-modifier (list 'STRICTFP))]]
    [genericInterfaceMethodDeclaration
     [((* interfaceMethodModifier) typeParameters interfaceCommonBodyDeclaration) (ast-generic-interface-method-declaration (list $1 $2 $3))]]
    [interfaceCommonBodyDeclaration
     [(primitiveType (*-prec typeType.3 EMPTY_BRACKETS) identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody)
      (make-interface-common-method (make-primitive-type $1 $2) $3 $4 $5 $6 $7)]
     [(IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody)
      (make-interface-common-method (make-reference-type $1 $2 $3 $4) $5 $6 $7 $8 $9)]
     [((* annotation) typeTypeOrVoid identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody) (ast-interface-common-body-declaration (list $1 $2 $3 $4 $5 $6 $7))]]
    [interfaceCommonBodyDeclaration.6
     [(THROWS qualifiedNameList) (ast-interface-common-body-declaration-throws (list 'THROWS $2))]]
    [variableDeclarators
     [((sep-by COMMA variableDeclarator)) $1]]
    [variableDeclarator
     [(variableDeclaratorId (? variableDeclarator.2)) (ast-variable-declarator (list $1 $2))]]
    [variableDeclarator.2
     [(ASSIGN variableInitializer) (ast-variable-declarator-initializer (list 'ASSIGN $2))]]
    [variableDeclaratorId
     [(identifier (* brackets)) (make-variable-declarator-id $1 $2)]]
    [variableInitializer
     [(arrayInitializer) (ast-variable-initializer (list $1))]
     [(expression) (ast-variable-initializer (list $1))]]
    [arrayInitializer
     [(LBRACE (? arrayInitializer.2) RBRACE) (ast-array-initializer (list 'LBRACE $2 'RBRACE))]]
    [arrayInitializer.2
     [((sep-by COMMA variableInitializer) (?->bool COMMA)) (ast-array-initializer-elements (list $1 $2))]]
    [classType
     [(typeIdentifier (? typeArguments) (* classType.2)) (ast-class-type (list $1 $2 $3))]]
    [classType.1
     [((? classType.1.1) typeIdentifier (? typeArguments)) (ast-class-type-segment (list $1 $2 $3))]]
    [classType.1.1
     [(packageName DOT (* annotation)) (ast-class-type-package-prefix (list $1 'DOT $3))]]
    [classType.2
     [(DOT (* annotation) typeIdentifier (? typeArguments)) (ast-class-type-suffix (list 'DOT $2 $3 $4))]]
    [packageName
     [((sep-by DOT identifier)) $1]]
    [typeArgument
     [(TYPE_IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS))
      (ast-type-argument (list (make-reference-type $1 $2 $3 $4)))]
     [(typeType) (ast-type-argument (list $1))]
     [((* annotation) QUESTION (? typeArgument.3)) (ast-type-argument (list $1 'QUESTION $3))]]
    [typeArgument.3
     [(typeArgument.3.1 typeType) (ast-type-argument-bound (list $1 $2))]]
    [typeArgument.3.1
     [(EXTENDS) 'extends]
     [(SUPER) 'super]]
    [qualifiedNameList
     [((sep-by COMMA qualifiedName)) $1]]
    [formalParameters
     [(LPAREN (? formalParameters.2) RPAREN) $2]]
    [formalParameters.2
     [(formalParameters.2.1 (* formalParameters.2.2)) (ast-formal-parameters-body (list $1 $2))]]
    [formalParameters.2.1
     [(qualifiedReceiverParameter) (ast-formal-parameters-first (list $1))]
     [((* annotation) IDENTIFIER THIS) (ast-formal-parameters-first (list (ast-receiver-parameter (list (make-annotated-reference-type $1 $2 '() '() '()) '() 'THIS))))]
     [(receiverParameter) (ast-formal-parameters-first (list $1))]
     [(formalParameter) (ast-formal-parameters-first (list $1))]]
    [formalParameters.2.2
     [(COMMA formalParameterList) $2]]
    [receiverParameter
     [((* annotation) IDENTIFIER THIS) (ast-receiver-parameter (list (make-annotated-reference-type $1 $2 '() '() '()) '() 'THIS))]
     [(typeType (* receiverParameter.2) THIS) (ast-receiver-parameter (list $1 $2 'THIS))]]
    [qualifiedReceiverParameter
     [(IDENTIFIER DOT IDENTIFIER IDENTIFIER DOT IDENTIFIER DOT THIS)
      (ast-receiver-parameter
       (list
        (make-reference-type
         $1
         '()
         (list (ast-class-type-suffix (list 'DOT '() $3 '())))
         '())
        (list (ast-receiver-parameter-qualifier (list $4 'DOT))
              (ast-receiver-parameter-qualifier (list $6 'DOT)))
        'THIS))]
     [(IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER THIS)
      (ast-receiver-parameter
       (list
        (make-reference-type
         $1
         '()
         (list (ast-class-type-suffix (list 'DOT '() $3 '()))
               (ast-class-type-suffix (list 'DOT '() $5 '())))
         '())
        '()
        'THIS))]
     [(IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER DOT THIS)
      (ast-receiver-parameter
       (list
        (make-reference-type
         $1
         '()
         (list (ast-class-type-suffix (list 'DOT '() $3 '()))
               (ast-class-type-suffix (list 'DOT '() $5 '())))
         '())
        (list (ast-receiver-parameter-qualifier (list $6 'DOT))
              (ast-receiver-parameter-qualifier (list $8 'DOT))
              (ast-receiver-parameter-qualifier (list $10 'DOT)))
        'THIS))]
     [(IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER IDENTIFIER DOT THIS)
      (ast-receiver-parameter
       (list
        (make-reference-type
         $1
         '()
         (list (ast-class-type-suffix (list 'DOT '() $3 '()))
               (ast-class-type-suffix (list 'DOT '() $5 '())))
         '())
        (list (ast-receiver-parameter-qualifier (list $6 'DOT)))
        'THIS))]]
    [receiverParameter.2
     [(identifier DOT) (ast-receiver-parameter-qualifier (list $1 'DOT))]]
    [formalParameterList
     [((sep-by COMMA formalParameter)) $1]]
    [formalParameter
     [(typeIdentifier (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) (? formalParameter.3) variableDeclaratorId)
      (ast-formal-parameter (list '() (make-reference-type $1 $2 $3 $4) $5 $6))]
     [(primitiveType (*-prec typeType.3 EMPTY_BRACKETS) (? formalParameter.3) variableDeclaratorId)
      (ast-formal-parameter (list '() (make-primitive-type $1 $2) $3 $4))]
     [((*-prec variableModifier FINAL) IDENTIFIER (+ typeType.3) identifier)
      (make-named-formal-parameter $1 (make-reference-type $2 '() '() $3) '() $4 '())]
     [((*-prec variableModifier FINAL) IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) identifier)
      (make-named-formal-parameter $1 (make-reference-type $2 $3 $4 $5) '() $6 '())]
     [(IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER typeArguments identifier)
      (ast-formal-parameter
       (list
        '()
        (make-reference-type
         $1
         '()
         (list (ast-class-type-suffix (list 'DOT '() $3 '()))
               (ast-class-type-suffix (list 'DOT '() $5 '()))
               (ast-class-type-suffix (list 'DOT '() $7 $8)))
         '())
        '()
        (make-variable-declarator-id $9 '())))]
     [(IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER typeArguments identifier)
      (ast-formal-parameter
       (list
        '()
        (make-reference-type
         $1
         '()
         (list (ast-class-type-suffix (list 'DOT '() $3 '()))
               (ast-class-type-suffix (list 'DOT '() $5 $6)))
         '())
        '()
        (make-variable-declarator-id $7 '())))]
     [(IDENTIFIER DOT IDENTIFIER typeArguments identifier)
      (ast-formal-parameter
       (list
        '()
        (make-reference-type
         $1
         '()
         (list (ast-class-type-suffix (list 'DOT '() $3 $4)))
         '())
        '()
        (make-variable-declarator-id $5 '())))]
     [(IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER identifier)
      (ast-formal-parameter
       (list
        '()
        (make-reference-type
         $1
         '()
         (list (ast-class-type-suffix (list 'DOT '() $3 '()))
               (ast-class-type-suffix (list 'DOT '() $5 '())))
         '())
        '()
        (make-variable-declarator-id $6 '())))]
     [((+ annotation) IDENTIFIER emptyBrackets identifier)
      (make-named-formal-parameter
       (map ast-variable-modifier $1)
       (make-reference-type $2 '() '() (make-empty-bracket-array-suffixes))
       '()
       $4
       '())]
     [((*-prec variableModifier FINAL) IDENTIFIER emptyBrackets identifier)
      (make-named-formal-parameter
       $1
       (make-reference-type $2 '() '() (make-empty-bracket-array-suffixes))
       '()
       $4
       '())]
     [((*-prec variableModifier FINAL) IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) ELLIPSIS variableDeclaratorId)
      (ast-formal-parameter (list
            $1
            (make-reference-type $2 $3 $4 $5)
            (ast-formal-parameter-varargs (list '() 'ELLIPSIS))
            $7))]
     [((*-prec variableModifier FINAL) typeType (? formalParameter.3) variableDeclaratorId) (ast-formal-parameter (list $1 $2 $3 $4))]]
    [lambdaLVTIList
     [((sep-by COMMA lambdaLVTIParameter)) $1]]
    [lambdaLVTIParameter
     [((*-prec variableModifier FINAL) VAR_DECL identifier) (ast-lambda-lvti-parameter (list $1 'VAR $3))]]
    [qualifiedName
     [((sep-by DOT identifier)) $1]]
    [literal
     [(integerLiteral) $1]
     [(floatLiteral) $1]
     [(CHAR_LITERAL) (ast-char-literal $1)]
     [(STRING_LITERAL) (ast-string-literal $1)]
     [(BOOL_LITERAL) (ast-bool-literal $1)]
     [(NULL_LITERAL) (ast-null-literal)]
     [(TEXT_BLOCK) (ast-text-block $1)]]
    [integerLiteral
     [(DECIMAL_LITERAL) (ast-integer-literal $1)]
     [(HEX_LITERAL) (ast-integer-literal $1)]
     [(OCT_LITERAL) (ast-integer-literal $1)]
     [(BINARY_LITERAL) (ast-integer-literal $1)]]
    [floatLiteral
     [(FLOAT_LITERAL) (ast-float-literal $1)]
     [(HEX_FLOAT_LITERAL) (ast-float-literal $1)]]
    [altAnnotationQualifiedName
     [((* altAnnotationQualifiedName.1) AT identifier) (ast-alt-annotation-qualified-name (list $1 'AT $3))]]
    [annotation
     [(annotation.1 (? annotationFieldValues)) (ast-annotation (list $1 $2))]]
    [annotation.1
     [(AT qualifiedName) $2]]
    [patternAnnotation
     [(PATTERN_AT qualifiedName (? annotationFieldValues)) (ast-annotation (list $2 $3))]
     [(annotation) $1]]
    [castAnnotation
     [(CAST_AT qualifiedName (? annotationFieldValues)) (ast-annotation (list $2 $3))]
     [(annotation) $1]]
    [varargsAnnotation
     [(VARARGS_AT qualifiedName (? annotationFieldValues)) (ast-annotation (list $2 $3))]
     [(annotation) $1]]
    [annotationFieldValues
     [(LPAREN (? (sep-by COMMA annotationFieldValue)) RPAREN) $2]]
    [annotationFieldValue
     [(identifier ASSIGN annotationValue) (ast-annotation-field-value (list $1 'ASSIGN $3))]
     [(annotationValue) (ast-annotation-field-value (list $1))]]
    [annotationValue
     [(expression) (ast-annotation-value (list $1))]
     [(annotation) (ast-annotation-value (list $1))]
     [(LBRACE (? (sep-by COMMA annotationValue)) (?->bool COMMA) RBRACE) $2]]
    [elementValue
     [(expression) (ast-element-value (list $1))]
     [(annotation) (ast-element-value (list $1))]
     [(elementValueArrayInitializer) (ast-element-value (list $1))]]
    [elementValueArrayInitializer
     [(LBRACE (? (sep-by COMMA elementValue)) (?->bool COMMA) RBRACE) (ast-element-value-array-initializer (list 'LBRACE $2 $3 'RBRACE))]]
    [annotationTypeDeclaration
     [(AT_INTERFACE identifier annotationTypeBody) (ast-annotation-type-declaration (list 'AT_INTERFACE $2 $3))]]
    [annotationTypeBody
     [(LBRACE (* annotationTypeElementDeclaration) RBRACE) (ast-annotation-type-body (list 'LBRACE $2 'RBRACE))]]
    [annotationTypeElementDeclaration
     [((* modifier) annotationTypeElementRest) (ast-annotation-type-element-declaration (list $1 $2))]
     [(SEMI) null]]
    [annotationTypeElementRest
     [(typeType annotationMethodOrConstantRest SEMI) (ast-annotation-type-element-rest (list $1 $2 'SEMI))]
     [(classDeclaration (?->bool SEMI)) $1]
     [(interfaceDeclaration (?->bool SEMI)) $1]
     [(enumDeclaration (?->bool SEMI)) $1]
     [(annotationTypeDeclaration (?->bool SEMI)) $1]
     [(recordDeclaration (?->bool SEMI)) $1]]
    [annotationMethodOrConstantRest
     [(annotationMethodRest) $1]
     [(annotationConstantRest) $1]]
    [annotationMethodRest
     [(identifier LPAREN RPAREN (? defaultValue)) (ast-annotation-method-rest (list $1 'LPAREN 'RPAREN $4))]]
    [annotationConstantRest
     [(variableDeclarators) (ast-annotation-constant-rest (list $1))]]
    [defaultValue
      [(DEFAULT elementValue) (ast-default-value (list 'DEFAULT $2))]]
    [moduleDeclaration
     [((* annotation) (?->bool OPEN) MODULE qualifiedName LBRACE (* moduleDirective) RBRACE) (ast-module-declaration (list $1 $2 'MODULE $4 'LBRACE $6 'RBRACE))]]
    [moduleDirective
     [(REQUIRES (* requiresModifier) qualifiedName SEMI) (ast-module-directive (list 'REQUIRES $2 $3 'SEMI))]
     [(EXPORTS qualifiedName (? moduleDirective.3) SEMI) (ast-module-directive (list 'EXPORTS $2 $3 'SEMI))]
     [(OPENS qualifiedName (? moduleDirective.3) SEMI) (ast-module-directive (list 'OPENS $2 $3 'SEMI))]
     [(USES qualifiedName SEMI) (ast-module-directive (list 'USES $2 'SEMI))]
     [(PROVIDES qualifiedName WITH (sep-by COMMA qualifiedName) SEMI) (ast-module-directive (list 'PROVIDES $2 'WITH $4 'SEMI))]]
    [moduleDirective.3
     [(TO (sep-by COMMA qualifiedName)) (ast-module-directive-to (list 'TO $2))]]
    [requiresModifier
     [(TRANSITIVE) 'transitive]
     [(STATIC) 'static]]
    [recordDeclaration
     [(RECORD identifier (? typeParameters) recordHeader (? recordDeclaration.5) recordBody) (ast-record-declaration (list 'RECORD $2 $3 $4 $5 $6))]]
    [recordDeclaration.5
     [(IMPLEMENTS typeList) $2]]
    [recordHeader
     [(LPAREN (? recordComponentList) RPAREN) $2]]
    [recordComponentList
     [((sep-by COMMA recordComponent)) $1]]
    [recordComponent
     [((* annotation) typeType (? recordComponent.3) identifier) (ast-record-component (list $1 $2 $3 $4))]]
    [recordComponent.3
     [((* annotation) ELLIPSIS) (ast-record-component-varargs (list $1 'ELLIPSIS))]]
    [recordBody
     [(LBRACE (* recordBody.2) RBRACE) (ast-record-body (list 'LBRACE $2 'RBRACE))]]
    [recordBody.2
     [((* modifier) constructorDeclaration) (ast-class-body-declaration (list $1 (ast-member-declaration (list $2))))]
     [(classBodyDeclaration) $1]
     [(compactConstructorDeclaration) $1]]
    [block
     [(LBRACE (* blockStatement) RBRACE) (ast-block (list $2))]]
    [blockStatement
     [(VAR_DECL identifier ASSIGN expression SEMI)
      (make-local-variable-block '() (make-var-type-rest $2 $4))]
     [(primitiveType (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI)
      (make-local-type-block '() (make-primitive-type $1 $2) $3)]
     [(IDENTIFIER typeArguments (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI)
      (make-reference-local-type-block $1 $2 $3 $4 $5)]
     [(OPENS typeArguments (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI)
      (make-reference-local-type-block "opens" $2 $3 $4 $5)]
     [(TYPE_IDENTIFIER DOT IDENTIFIER typeArguments (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI)
      (make-reference-local-type-block
       $1
       '()
       (cons (ast-class-type-suffix (list 'DOT '() $3 $4)) $5)
       $6
       $7)]
     [(TYPE_IDENTIFIER DOT IDENTIFIER (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI)
      (make-reference-local-type-block
       $1
       '()
       (cons (ast-class-type-suffix (list 'DOT '() $3 '())) $4)
       $5
       $6)]
     [(typeIdentifier typeArguments (+ classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI)
      (make-reference-local-type-block $1 $2 $3 $4 $5)]
     [(IDENTIFIER (? typeArguments) (+ classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI)
      (make-reference-local-type-block $1 $2 $3 $4 $5)]
     [(IDENTIFIER (? typeArguments) (* classType.2) (+ typeType.3) variableDeclarators SEMI)
      (make-reference-local-type-block $1 $2 $3 $4 $5)]
     [(typeIdentifier (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI)
      (make-reference-local-type-block $1 $2 $3 $4 $5)]
     [(localVariableDeclaration SEMI) (ast-block-statement (list $1))]
     [(localTypeDeclaration) $1]
     [(statement) $1]]
    [localVariableDeclaration
     [((*-prec variableModifier FINAL) localVariableDeclaration.2) (ast-local-variable-declaration (list $1 $2))]]
    [localVariableDeclaration.2
     [(VAR_DECL identifier ASSIGN expression) (make-var-type-rest $2 $4)]
     [(typeType variableDeclarators) (ast-local-variable-declaration-rest (list $1 $2))]]
    [identifier
     [(IDENTIFIER) $1]
     [(MODULE) "module"]
     [(OPEN) "open"]
     [(REQUIRES) "requires"]
     [(EXPORTS) "exports"]
     [(OPENS) "opens"]
     [(TO) "to"]
     [(USES) "uses"]
     [(PROVIDES) "provides"]
     [(WHEN) "when"]
     [(WITH) "with"]
     [(TRANSITIVE) "transitive"]
     [(YIELD) "yield"]
     [(SEALED) "sealed"]
     [(PERMITS) "permits"]
     [(RECORD) "record"]
     [(VAR) "var"]]
    [typeIdentifier
     [(IDENTIFIER) $1]
     [(TYPE_IDENTIFIER) $1]
     [(MODULE) "module"]
     [(OPEN) "open"]
     [(REQUIRES) "requires"]
     [(EXPORTS) "exports"]
     [(OPENS) "opens"]
     [(TO) "to"]
     [(USES) "uses"]
     [(PROVIDES) "provides"]
     [(WITH) "with"]
     [(TRANSITIVE) "transitive"]
     [(SEALED) "sealed"]]
    [localTypeDeclaration
     [((* classOrInterfaceModifier) localTypeDeclaration.2) (ast-local-type-declaration (list $1 $2))]]
    [localTypeDeclaration.2
     [(classDeclaration) $1]
     [(interfaceDeclaration) $1]
     [(recordDeclaration) $1]
     [(enumDeclaration) $1]]
    [statement
     [(block) $1] ;blockLabel = block
     [(ASSERT expression (? statement.3/2) SEMI) (ast-statement (list 'ASSERT $2 $3 'SEMI))]
     [(IF LPAREN expression RPAREN statement (? statement.6)) (ast-statement (list 'IF 'LPAREN $3 'RPAREN $5 $6))]
     [(FOR LPAREN forControl RPAREN statement) (ast-statement (list 'FOR 'LPAREN $3 'RPAREN $5))]
     [(WHILE LPAREN expression RPAREN statement) (ast-statement (list 'WHILE 'LPAREN $3 'RPAREN $5))]
     [(DO statement WHILE LPAREN expression RPAREN SEMI) (ast-statement (list 'DO $2 'WHILE 'LPAREN $5 'RPAREN 'SEMI))]
     [(TRY block statement.3/7) (ast-statement (list 'TRY $2 $3))]
     [(TRY resourceSpecification block (* catchClause) (? finallyBlock)) (ast-statement (list 'TRY $2 $3 $4 $5))]
     [(SWITCH LPAREN expression RPAREN LBRACE (* switchBlockStatementGroup) (* switchLabel) RBRACE) (ast-statement (list 'SWITCH 'LPAREN $3 'RPAREN 'LBRACE $6 $7 'RBRACE))]
     [(SYNCHRONIZED LPAREN expression RPAREN block) (ast-statement (list 'SYNCHRONIZED 'LPAREN $3 'RPAREN $5))]
     [(RETURN (? expression) SEMI) (ast-statement (list 'RETURN $2 'SEMI))]
     [(THROW expression SEMI) (ast-statement (list 'THROW $2 'SEMI))]
     [(BREAK (? identifier) SEMI) (ast-statement (list 'BREAK $2 'SEMI))]
     [(CONTINUE (? identifier) SEMI) (ast-statement (list 'CONTINUE $2 'SEMI))]
     [(YIELD expression SEMI) (ast-statement (list 'YIELD $2 'SEMI))]
     [(SEMI) null]
     [(expression SEMI) (ast-statement (list $1 'SEMI))] ;statementExpression = expression
     [(switchExpression (?->bool SEMI)) (ast-statement (list $1 $2))]
     [(identifier COLON statement) (ast-statement (list $1 'COLON $3))]] ;identifierLabel = identifier
    [statement.3/2
     [(COLON expression) $2]]
    [statement.6
     [(ELSE statement) $2]]
    [statement.3/7
     [((+ catchClause) (? finallyBlock)) (ast-statement-try-rest (list $1 $2))]
     [(finallyBlock) (ast-statement-try-rest (list $1))]]
    [catchClause
     [(CATCH LPAREN (*-prec variableModifier FINAL) catchType identifier RPAREN block) (ast-catch-clause (list 'CATCH 'LPAREN $3 $4 $5 'RPAREN $7))]]
    [catchType
     [((sep-by BITOR qualifiedName)) $1]]
    [finallyBlock
     [(FINALLY block) (ast-finally-block (list 'FINALLY $2))]]
    [resourceSpecification
     [(LPAREN resources (?->bool SEMI) RPAREN) (ast-resource-specification (list 'LPAREN $2 $3 'RPAREN))]]
    [resources
     [((sep-by SEMI resource)) $1]]
    [resource
     [((*-prec variableModifier FINAL) VAR_DECL identifier ASSIGN expression) (ast-resource (list $1 'VAR $3 'ASSIGN $5))]
     [(IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclaratorId ASSIGN expression)
      (ast-resource (list '() (ast-class-or-interface-type (list (ast-class-type (list $1 $2 $3)))) $5 'ASSIGN $7))]
     [((*-prec variableModifier FINAL) classOrInterfaceType variableDeclaratorId ASSIGN expression) (ast-resource (list $1 $2 $3 'ASSIGN $5))]
     [(qualifiedName) (ast-resource (list $1))]]
    [switchBlockStatementGroup
     [((+ switchBlockStatementGroup.1) (+ blockStatement)) (ast-switch-block-statement-group (list $1 $2))]]
    [switchBlockStatementGroup.1
     [(switchLabel COLON) (ast-switch-block-statement-group-label (list $1 'COLON))]]
    [switchLabel
     [(CASE expression) (ast-switch-label (list 'CASE $2))]
     [(CASE IDENTIFIER) (ast-switch-label (list 'CASE $2))]
     [(CASE INSTANCEOF_PATTERN_TYPE (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) identifier)
      (ast-switch-label (list 'CASE (make-type-identifier-pattern (make-reference-type $2 $3 $4 $5) $6)))]
     [(CASE typeType identifier) (ast-switch-label (list 'CASE $2 $3))]
     [(DEFAULT) 'default]]
    [forControl
     [(forPrimitiveLocalVariableDeclaration SEMI (? expression) SEMI (? expressionList))
      (ast-for-control
       (list
        (ast-for-init (list $1))
        'SEMI
        $3
        'SEMI
        $5))]
     [(IDENTIFIER typeArguments (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators SEMI (? expression) SEMI (? expressionList))
      (make-for-reference-local-type-control $1 $2 $3 $4 $5 $7 $9)]
     [((*-prec variableModifier FINAL) VAR_DECL variableDeclaratorId COLON expression) (ast-for-control (list (ast-enhanced-for-control (list $1 'VAR $3 'COLON $5))))]
     [(PRIMITIVE_ENHANCED (*-prec typeType.3 EMPTY_BRACKETS) variableDeclaratorId COLON expression)
      (make-enhanced-for-type-control '() (make-primitive-type $1 $2) $3 $5)]
     [(IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) variableDeclaratorId COLON expression)
      (make-reference-enhanced-for-control '() $1 $2 $3 $4 $5 $7)]
     [((*-prec variableModifier FINAL) primitiveType (*-prec typeType.3 EMPTY_BRACKETS) variableDeclaratorId COLON expression)
      (make-enhanced-for-type-control $1 (make-primitive-type $2 $3) $4 $6)]
     [(enhancedForControl) (ast-for-control (list $1))]
     [((? forInit) SEMI (? expression) SEMI (? expressionList)) (ast-for-control (list $1 'SEMI $3 'SEMI $5))]]
    [forPrimitiveLocalVariableDeclaration
     [((*-prec variableModifier FINAL) forPrimitiveLocalVariableDeclaration.2)
      (ast-local-variable-declaration (list $1 $2))]]
    [forPrimitiveLocalVariableDeclaration.2
     [(PRIMITIVE_DECL (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest $1 $2 $3)]
     [(BOOLEAN (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest 'boolean $2 $3)]
     [(CHAR (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest 'char $2 $3)]
     [(BYTE (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest 'byte $2 $3)]
     [(SHORT (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest 'short $2 $3)]
     [(INT (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest 'int $2 $3)]
     [(LONG (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest 'long $2 $3)]
     [(FLOAT (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest 'float $2 $3)]
     [(DOUBLE (*-prec typeType.3 EMPTY_BRACKETS) variableDeclarators) (make-primitive-local-type-rest 'double $2 $3)]]
    [forInit
     [(localVariableDeclaration) (ast-for-init (list $1))]
     [(expressionList) (ast-for-init (list $1))]]
    [enhancedForControl
     [((*-prec variableModifier FINAL) typeType variableDeclaratorId COLON expression) (ast-enhanced-for-control (list $1 $2 $3 'COLON $5))]
     [((*-prec variableModifier FINAL) VAR_DECL variableDeclaratorId COLON expression) (ast-enhanced-for-control (list $1 'VAR $3 'COLON $5))]]
    [expressionList
     [((sep-by COMMA expression)) $1]]
    [methodCall
     [(identifier arguments) (ast-method-call (list $1 $2))]
     [(THIS arguments) (ast-method-call (list 'THIS $2))]
     [(SUPER arguments) (ast-method-call (list 'SUPER $2))]]
    [expression
     [(lambdaExpression) (ast-expression (list $1))]
     [(assignmentExpression) $1]]
    [assignmentExpression
     [(conditionalExpression) (prec ASSIGN) $1]
     [(conditionalExpression expression.39 expression) (ast-expression (list $1 $2 $3))]]
    [conditionalExpression
     [(logicalOrExpression) (prec QUESTION) $1]
     [(logicalOrExpression QUESTION expression COLON expression) (ast-expression (list $1 'QUESTION $3 'COLON $5))]]
    [logicalOrExpression
     [(logicalAndExpression) (prec OR) $1]
     [(logicalOrExpression OR logicalAndExpression) (ast-expression (list $1 'OR $3))]]
    [logicalAndExpression
     [(inclusiveOrExpression) (prec AND) $1]
     [(logicalAndExpression AND inclusiveOrExpression) (ast-expression (list $1 'AND $3))]]
    [inclusiveOrExpression
     [(exclusiveOrExpression) (prec BITOR) $1]
     [(inclusiveOrExpression BITOR exclusiveOrExpression) (ast-expression (list $1 'BITOR $3))]]
    [exclusiveOrExpression
     [(andExpression) (prec CARET) $1]
     [(exclusiveOrExpression CARET andExpression) (ast-expression (list $1 'CARET $3))]]
    [andExpression
     [(equalityExpression) (prec BITAND) $1]
     [(andExpression BITAND equalityExpression) (ast-expression (list $1 'BITAND $3))]]
    [equalityExpression
     [(relationalExpression) (prec EQUAL) $1]
     [(equalityExpression expression.32 relationalExpression) (ast-expression (list $1 $2 $3))]]
    [relationalExpression
     [(shiftExpression) (prec LT_SINGLE) $1]
     [(relationalExpression expression.29 shiftExpression) (ast-expression (list $1 $2 $3))]
     [(relationalExpression INSTANCEOF FINAL (*-prec variableModifier FINAL) typeType (* annotation) variableDeclarators)
      (ast-expression (list $1 'INSTANCEOF (ast-pattern (list (cons 'final $4) $5 $6 $7))))]
     [(relationalExpression INSTANCEOF (+ patternAnnotation) FINAL (*-prec variableModifier FINAL) typeType (* annotation) variableDeclarators)
      (ast-expression (list $1 'INSTANCEOF (ast-pattern (list (append $3 (cons 'final $5)) $6 $7 $8))))]
     [(relationalExpression INSTANCEOF (+ patternAnnotation) IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) identifier)
      (ast-expression
       (list
        $1
        'INSTANCEOF
        (ast-pattern
         (list
          $3
          (make-reference-type $4 $5 $6 $7)
          '()
          (list
           (ast-variable-declarator
            (list (ast-variable-declarator-id (list $8 '())) '())))))))]
     [(relationalExpression INSTANCEOF INSTANCEOF_PATTERN_TYPE (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) identifier)
      (ast-expression (list $1 'INSTANCEOF (make-type-identifier-pattern (make-reference-type $3 $4 $5 $6) $7)))]
     [(relationalExpression INSTANCEOF typeType) (ast-expression (list $1 'INSTANCEOF $3))]]
    [shiftExpression
     [(additiveExpression) (prec LT) $1]
     [(shiftExpression expression.28 additiveExpression) (ast-expression (list $1 $2 $3))]]
    [additiveExpression
     [(multiplicativeExpression) (prec ADD) $1]
     [(additiveExpression expression.27 multiplicativeExpression) (ast-expression (list $1 $2 $3))]]
    [multiplicativeExpression
     [(unaryExpression) (prec MUL) $1]
     [(multiplicativeExpression expression.26 unaryExpression) (ast-expression (list $1 $2 $3))]]
    [unaryExpression
     [(expression.20 unaryExpression) (ast-expression (list $1 $2))]
     [(LPAREN primitiveType (*-prec typeType.3 EMPTY_BRACKETS) RPAREN expression)
      (ast-expression
       (list 'LPAREN
             '()
             (ast-type-type (list '() (ast-type-base (list $2)) $3))
             '()
             'RPAREN
             $5))]
     [(LPAREN (+ castAnnotation) primitiveType (*-prec typeType.3 EMPTY_BRACKETS) RPAREN expression)
      (ast-expression
       (list 'LPAREN
             '()
             (make-annotated-primitive-type $2 $3 $4)
             '()
             'RPAREN
             $6))]
     [(LPAREN IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) (+ expression.23) RPAREN lambdaExpression)
      (ast-expression
       (list 'LPAREN
             '()
             (make-reference-type $2 $3 $4 $5)
             $6
             'RPAREN
             (ast-expression (list $8))))]
     [(LPAREN (+ annotation) typeIdentifier (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) (* expression.23) RPAREN expression)
      (ast-expression
       (list 'LPAREN
             '()
             (make-annotated-reference-type $2 $3 $4 $5 $6)
             $7
             'RPAREN
             $9))]
     [(LPAREN (+ castAnnotation) typeIdentifier (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) (* expression.23) RPAREN expression)
      (ast-expression
       (list 'LPAREN
             '()
             (make-annotated-reference-type $2 $3 $4 $5 $6)
             $7
             'RPAREN
             $9))]
     [(LPAREN typeIdentifier (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) (* expression.23) RPAREN expression)
      (ast-expression
       (list 'LPAREN
             '()
             (ast-type-type
              (list
               '()
               (ast-type-base
                (list
                 (ast-class-or-interface-type
                  (list (ast-class-type (list $2 $3 $4))))))
               $5))
             $6
             'RPAREN
             $8))]
     [(LPAREN typeIdentifier (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) (* expression.23) RPAREN identifier COLONCOLON (? typeArguments) identifier)
      (ast-expression
       (list 'LPAREN
             '()
             (make-reference-type $2 $3 $4 $5)
             $6
             'RPAREN
             (make-simple-method-reference $8 $10 $11)))]
     [(LPAREN (* annotation) typeType (* expression.23) RPAREN expression) (ast-expression (list 'LPAREN $2 $3 $4 'RPAREN $6))]
     [(postfixExpression) $1]]
    [postfixExpression
     [(postfixBase (* postfixSuffix) (? expression.18)) (apply-postfix-suffixes $1 $2 $3)]]
    [postfixBase
     [(primary) (ast-expression (list $1))]
     [(methodCall) (ast-expression (list $1))]
     [((* annotation) IDENTIFIER typeArguments (+ classType.2) COLONCOLON NEW)
      (ast-expression (list (make-annotated-reference-type $1 $2 $3 $4 '()) 'COLONCOLON 'NEW))]
     [(IDENTIFIER (+ typeType.3) COLONCOLON NEW)
      (ast-expression (list (make-reference-type $1 '() '() $2) 'COLONCOLON 'NEW))]
     [(switchExpression) (ast-expression (list $1))]
     [(NEW creator) (ast-expression (list 'NEW $2))]]
    [postfixSuffix
     [(LBRACK expression RBRACK) (list 'LBRACK $2 'RBRACK)]
     [(DOT identifier) (list 'DOT $2)]
     [(DOT methodCall) (list 'DOT $2)]
     [(DOT CLASS) (list 'DOT 'CLASS)]
     [(DOT THIS) (list 'DOT 'THIS)]
     [(DOT NEW (? nonWildcardTypeArguments) innerCreator) (list 'DOT 'NEW $3 $4)]
     [(DOT SUPER superSuffix) (list 'DOT 'SUPER $3)]
     [(DOT explicitGenericInvocation) (list 'DOT $2)]
     [(COLONCOLON (? typeArguments) identifier) (list 'COLONCOLON $2 $3)]
     [(COLONCOLON (? typeArguments) NEW) (list 'COLONCOLON $2 'NEW)]]
    [expression.18
     [(INC) 'inc]
     [(DEC) 'dec]]
    [expression.20
     [(ADD) 'pos]
     [(SUB) 'neg]
     [(INC) 'inc]
     [(DEC) 'dec]
     [(TILDE) 'bnot]
     [(BANG) 'lnot]]
    [expression.23
     [(BITAND typeType) (ast-expression-intersection-type (list 'BITAND $2))]]
    [expression.26
     [(MUL) 'mul]
     [(DIV) 'div]
     [(MOD) 'mod]]
    [expression.27
     [(ADD) 'add]
     [(SUB) 'sub]]
    [expression.28
     [(LT LT_SINGLE) 'shl]
     [(GT GT GT_SINGLE) 'ushr]
     [(GT GT_SINGLE) 'shr]]
    [expression.29
     [(LE) 'le]
     [(GE) 'ge]
     [(GT_SINGLE) 'gt]
     [(LT_SINGLE) 'lt]]
    [expression.32
     [(EQUAL) 'equal]
     [(NOTEQUAL) 'notequal]]
    [expression.39
     [(ASSIGN) 'assign]
     [(ADD_ASSIGN) 'add-assign]
     [(SUB_ASSIGN) 'sub-assign]
     [(MUL_ASSIGN) 'mul-assign]
     [(DIV_ASSIGN) 'div-assign]
     [(AND_ASSIGN) 'and-assign]
     [(OR_ASSIGN) 'or-assign]
     [(XOR_ASSIGN) 'xor-assign]
     [(RSHIFT_ASSIGN) 'rshift-assign]
     [(URSHIFT_ASSIGN) 'urshift-assign]
     [(LSHIFT_ASSIGN) 'lshift-assign]
     [(MOD_ASSIGN) 'mod-assign]]
    [pattern
      [((*-prec variableModifier FINAL) typeType (* annotation) variableDeclarators) (ast-pattern (list $1 $2 $3 $4))]
      [(typeType LPAREN (? componentPatternList) RPAREN) (ast-pattern (list $1 'LPAREN $3 'RPAREN))]]
    [componentPatternList
     [((sep-by COMMA componentPattern)) $1]]
    [componentPattern
     [(pattern) (ast-component-pattern (list $1))]]
    [lambdaExpression
     [(lambdaParameters ARROW lambdaBody) (ast-lambda-expression (list $1 'ARROW $3))]]
    [lambdaParameters
     [(identifier) (ast-lambda-parameters (list $1))]
     [(LPAREN (? formalParameterList) RPAREN) (ast-lambda-parameters (list 'LPAREN $2 'RPAREN))]
     [(LPAREN (sep-by COMMA identifier) RPAREN) (ast-lambda-parameters (list 'LPAREN $2 'RPAREN))]
     [(LPAREN (? lambdaLVTIList) RPAREN) (ast-lambda-parameters (list 'LPAREN $2 'RPAREN))]]
    [lambdaBody
     [(expression) (ast-lambda-body (list $1))]
     [(block) (ast-lambda-body (list $1))]]
    [primary
     [(LPAREN expression RPAREN) (ast-primary (list 'LPAREN $2 'RPAREN))]
     [(THIS) 'this]
     [(SUPER) 'super]
     [(literal) (ast-primary (list $1))]
     [(identifier) (ast-primary (list $1))]
     [(typeTypeOrVoid DOT CLASS) (ast-primary (list $1 'DOT 'CLASS))]
     [(nonWildcardTypeArguments primary.7) (ast-primary (list $1 $2))]]
    [primary.7
     [(explicitGenericInvocationSuffix) (ast-primary-generic-suffix (list $1))]
     [(THIS arguments) (ast-primary-generic-suffix (list 'THIS $2))]]
    [switchExpression
     [(SWITCH LPAREN expression RPAREN LBRACE (* switchLabeledRule) RBRACE) (ast-switch-expression (list 'SWITCH 'LPAREN $3 'RPAREN 'LBRACE $6 'RBRACE))]]
    [switchLabeledRule
     [(CASE NULL_LITERAL switchLabeledRule.2.2 switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE 'NULL_LITERAL $3 $4 $5))]
     [(CASE IDENTIFIER switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE (list (make-identifier-expression $2)) $3 $4))]
     [(CASE VAR switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE (list (make-identifier-expression "var")) $3 $4))]
     [(CASE INSTANCEOF_PATTERN_TYPE (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) identifier (? guard) switchLabeledRule.3 switchRuleOutcome)
      (ast-switch-labeled-rule (list 'CASE (make-type-identifier-pattern (make-reference-type $2 $3 $4 $5) $6) $7 $8 $9))]
     [(CASE typeType identifier (? guard) switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE (make-type-identifier-pattern $2 $3) $4 $5 $6))]
     [(CASE (sep-by COMMA casePattern) (? guard) switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE $2 $3 $4 $5))]
     [(CASE switchIdentifierLabels switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE $2 $3 $4))]
     [(CASE expressionList switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE $2 $3 $4))]
     [(DEFAULT switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'DEFAULT $2 $3))]]
    [switchIdentifierLabels
     [(identifier) (list (make-identifier-expression $1))]
     [(identifier COMMA switchIdentifierLabels) (cons (make-identifier-expression $1) $3)]]
    [switchLabeledRule.2.2
     [() #f]
     [(COMMA DEFAULT) #t]]
    [switchLabeledRule.3
     [(ARROW) 'arrow]
     [(COLON) 'colon]]
    [guard
     [(WHEN expression) (ast-guard (list 'WHEN $2))]]
    [casePattern
     [(pattern) (ast-case-pattern (list $1))]]
    [switchRuleOutcome
     [(block) (ast-switch-rule-outcome (list $1))]
     [(expression SEMI)
      (ast-switch-rule-outcome
       (list (list (ast-block-statement (list (ast-statement (list $1 'SEMI)))))))]
     [((* blockStatement)) (ast-switch-rule-outcome (list $1))]]
    [classOrInterfaceType
     [(classType) (ast-class-or-interface-type (list $1))]]
    [creator
     [(createdName creatorRest) (ast-creator (list $1 $2))]
     [(nonWildcardTypeArguments createdName classCreatorRest) (ast-creator (list $1 $2 $3))]]
    [creatorRest
     [(classCreatorRest) (ast-creator-rest (list $1))]
     [(arrayCreatorRest) (ast-creator-rest (list $1))]]
    [createdName
     [(identifier (? typeArgumentsOrDiamond) (* createdName.3)) (ast-created-name (list $1 $2 $3))]
     [(primitiveType) (ast-created-name (list $1))]]
    [createdName.3
     [(DOT identifier (? typeArgumentsOrDiamond)) (ast-created-name-suffix (list 'DOT $2 $3))]]
    [innerCreator
     [(identifier (? nonWildcardTypeArgumentsOrDiamond) classCreatorRest) (ast-inner-creator (list $1 $2 $3))]]
    [arrayCreatorRest
     [((+ brackets) arrayInitializer) (ast-array-creator-rest (list $1 $2))]
     [((+ arrayCreatorRest.2) (* brackets)) (ast-array-creator-rest (list $1 $2))]]
    [arrayCreatorRest.2
     [(LBRACK expression RBRACK) (ast-array-creator-rest-sized-dimension (list 'LBRACK $2 'RBRACK))]]
    [classCreatorRest
     [(arguments (? classBody)) (ast-class-creator-rest (list $1 $2))]]
    [explicitGenericInvocation
     [(nonWildcardTypeArguments explicitGenericInvocationSuffix) (ast-explicit-generic-invocation (list $1 $2))]]
    [typeArgumentsOrDiamond
     [(lt gt) (ast-type-arguments-or-diamond (list 'LT 'GT))]
     [(typeArguments) (ast-type-arguments-or-diamond (list $1))]]
    [nonWildcardTypeArgumentsOrDiamond
     [(lt gt) (ast-non-wildcard-type-arguments-or-diamond (list 'LT 'GT))]
     [(nonWildcardTypeArguments) (ast-non-wildcard-type-arguments-or-diamond (list $1))]]
    [nonWildcardTypeArguments
     [(lt typeList gt) (ast-non-wildcard-type-arguments (list 'LT $2 'GT))]]
    [typeList
     [((sep-by COMMA typeType)) $1]]
    [typeType
     [((* annotation) typeType.2 (*-prec typeType.3 EMPTY_BRACKETS)) (ast-type-type (list $1 $2 $3))]]
    [typeType.2
     [(classOrInterfaceType) (ast-type-base (list $1))]
     [(primitiveType) (ast-type-base (list $1))]]
    [typeType.3
     [((* annotation) emptyBrackets) (ast-type-array-suffix (list $1 'LBRACK 'RBRACK))]]
    [emptyBrackets
     [(LBRACK RBRACK) '()]
     [(EMPTY_BRACKETS) '()]]
    [primitiveType
     [(BOOLEAN) 'boolean]
     [(CHAR) 'char]
     [(BYTE) 'byte]
     [(SHORT) 'short]
     [(INT) 'int]
     [(LONG) 'long]
     [(FLOAT) 'float]
     [(DOUBLE) 'double]]
    [typeArguments
     [(lt (sep-by COMMA typeArgument) gt) $2]
     [(lt typeArgumentWithClose) (list $2)]]
    [typeArgumentWithClose
     [(TYPE_IDENTIFIER (? typeArguments) (* classType.2) (*-prec typeType.3 EMPTY_BRACKETS) gt)
      (ast-type-argument-with-close (list (make-reference-type $1 $2 $3 $4) 'GT))]
     [(typeType gt) (ast-type-argument-with-close (list $1 'GT))]]
    [lt
     [(LT) 'LT]]
    [gt
     [(GT) 'GT]
     [(GT_SINGLE) 'GT]]
    [superSuffix
     [(arguments) (ast-super-suffix (list $1))]
     [(DOT (? typeArguments) identifier (? arguments)) (ast-super-suffix (list 'DOT $2 $3 $4))]]
    [explicitGenericInvocationSuffix
     [(SUPER superSuffix) (ast-explicit-generic-invocation-suffix (list 'SUPER $2))]
     [(identifier arguments) (ast-explicit-generic-invocation-suffix (list $1 $2))]]
    [arguments
     [(LPAREN (? expressionList) RPAREN) $2]]
    [formalParameter.3
     [((* annotation) ELLIPSIS) (ast-formal-parameter-varargs (list $1 'ELLIPSIS))]
     [((* varargsAnnotation) ELLIPSIS) (ast-formal-parameter-varargs (list $1 'ELLIPSIS))]]
    [enumDeclaration.3
     [(IMPLEMENTS typeList) (ast-enum-declaration-implements (list 'IMPLEMENTS $2))]]
    [altAnnotationQualifiedName.1
     [(identifier DOT) (ast-alt-annotation-qualified-name-prefix (list $1 'DOT))]]]))

(define (parse-java-code input)
  (define port (open-input-string input))
  (port-count-lines! port)
  (java-parser (make-next-significant-token port)))

(define (parse-java-file path)
  (call-with-input-file path
    (lambda (port)
      (port-count-lines! port)
      (java-parser (make-next-significant-token port)))))
