#lang racket/base
(require parser-tools/lex
         "lexer.rkt"
         "ext-parser.rkt"
         "ast.rkt")

(define java-parser
  (ext-parser
   [start packageName]
   [end EOF]
   [error (lambda (tok-ok? tok-name tok-value start end)
            (error 'java-parser "Parse error at line ~a, col ~a: ~a ~a"
                   (position-line start) (position-col start)
                   tok-name tok-value))]
   [src-pos]
   [tokens empty-tokens tokens]
   [grammar
    [variableDeclaratorId ;257
     [(identifier variableDeclaratorId.2) ]]
    [packageName ;276
     [(identifier packageName.2*) (cons $1 $2)]]
    [packageName.2
     [(DOT identifier) $2]]
    [qualifiedNameList ;285
     [(qualifiedName qualifiedNameList.2*) (cons $1 $2)]]
    [qualifiedNameList.2
     [(COMMA qualifiedName) $2]]
    [qualifiedName ;316
     [(identifier qualifiedName.2*) (cons $1 $2)]]
    [qualifiedName.2
     [(DOT identifier) $2]]
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
    [floatLiteral ;337
     [(FLOAT_LITERAL) (ast-float-literal $1)]
     [(HEX_FLOAT_LITERAL) (ast-float-literal $1)]]
    [identifier ;480
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
    [primary ;710*
     [(THIS) 'this]
     [(SUPER) 'super]
     [(literal) $1]
     [(identifier) $1]]
    [primitiveType ;799
     [(BOOLEAN) 'boolean]
     [(CHAR) 'char]
     [(BYTE) 'byte]
     [(SHORT) 'short]
     [(INT) 'int]
     [(LONG) 'long]
     [(FLOAT) 'float]
     [(DOUBLE) 'double]]
    ]))
(define (parse-java-code input)
  (define port (open-input-string input))
  (java-parser (lambda () (java-lexer port))))
(module+ main
  (require racket/pretty)
  (pretty-print (parse-java-code "ab.dd2.to")))