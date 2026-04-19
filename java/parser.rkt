#lang racket/base
(require parser-tools/lex
         "lexer.rkt"
         "ext-parser.rkt"
         "ast.rkt")
(define-grammar-operator (? s)
  [main
   [() '()]
   [(s) $1]])
(define-grammar-operator (* s)
  [main
   [() '()]
   [(s main) (cons $1 $2)]])
(define-grammar-operator (+ s)
  [main
   [(s) (cons $1 '())]
   [(s main) (cons $1 $2)]])
(define-grammar-operator (sep-by separator element)
  [main
   [(element (* rest)) (cons $1 $2)]]
  [rest
   [(separator element) $1]])
(define todo null)
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
   [grammar
    [compilationUnit
     [((? packageDeclaration) (* compilationUnit.2) (* compilationUnit.3)) todo]
     [(modularCompulationUnit) todo]]
    [compilationUnit.2
     [(importDeclaration) todo]
     [(SEMI) '()]]
    [compilationUnit.3
     [(typeDeclaration) todo]
     [(SEMI) '()]]
    [modularCompulationUnit
     [((* importDeclaration) moduleDeclaration) todo]]
    [packageDeclaration
     [((* annotation) PACKAGE qualifiedName SEMI) todo]]
    [importDeclaration
     [(IMPORT (? importDeclaration.2) qualifiedName importDeclaration.4 SEMI) todo]]
    [importDeclaration.2
     [(STATIC) #t]
     [() #f]]
    [importDeclaration.4
     [(DOT MUL) #t]
     [() #f]]
    [typeDeclaration
     [((* classOrInterfaceModifier) typeDeclaration.2) todo]]
    [typeDeclaration.2
     [(classDeclaration) todo]
     [(enumDeclaration) todo]
     [(interfaceDeclaration) todo]
     [(annotationTypeDeclaration) todo]
     [(recordDeclaration) todo]]
    [modifier
     [(classOrInterfaceModifier) todo]
     [(NATIVE) 'native]
     [(SYNCHRONIZED) 'synchronized]
     [(TRANSIENT) 'transient]
     [(VOLATILE) 'volatile]]
    [classOrInterfaceModifier
     [(annotation) todo]
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
     [(annotation) todo]]
    [classDeclaration
     [(CLASS identifier (? typeParameters) (? classDeclaration.4) (? classDeclaration.5) (? classDeclaration.6) classBody) todo]]
    [classDeclaration.4
     [(EXTENDS typeType) todo]]
    [classDeclaration.5
     [(IMPLEMENTS typeList) todo]]
    [classDeclaration.6
     [(PERMITS typeList) todo]]
    [typeParameters
     [(LT (sep-by COMMA typeParameter) GT) todo]]
    [typeParameter
     [((* annotation) identifier (? typeParameter.3)) todo]]
    [typeParameter.3
     [(EXTENDS (* annotation) typeBound) todo]]
    [typeBound
     [((sep-by BITAND typeType)) todo]]
    [enumDeclaration
     [(ENUM identifier (? enumDeclaration.3) LBRACE (? enumConstants) (? enumDeclaration.6) (? enumBodyDeclarations) RBRACE) todo]]
    [enumConstants
     [((sep-by COMMA enumConstant)) todo]]
    [enumConstant
     [((* annotation) identifier (? arguments) (? classBody)) todo]]
    [enumBodyDeclarations
     [(SEMI (* classBodyDeclaration)) todo]]
    [interfaceDeclaration
     [(INTERFACE identifier (? typeParameters) (? interfaceDeclaration.4) (? interfaceDeclaration.5) interfaceBody) todo]]
    [interfaceDeclaration.4
     [(EXTENDS typeList) todo]]
    [interfaceDeclaration.5
     [(PERMITS typeList) todo]]
    [classBody
     [(LBRACE (* classBodyDeclaration) RBRACE) todo]]
    [interfaceBody
     [(LBRACE (* interfaceBodyDeclaration) RBRACE) todo]]
    [classBodyDeclaration
     [(SEMI) todo]
     [(classBodyDeclaration.1 block) todo]
     [((* modifier) memberDeclaration) todo]]
    [classBodyDeclaration.1
     [(STATIC) #t]
     [() #f]]
    [memberDeclaration
     [(recordDeclaration) todo]
     [(methodDeclaration) todo]
     [(genericMethodDeclaration) todo]
     [(fieldDeclaration) todo]
     [(constructorDeclaration) todo]
     [(genericConstructorDeclaration) todo]
     [(interfaceDeclaration) todo]
     [(annotationTypeDeclaration) todo]
     [(classDeclaration) todo]
     [(enumDeclaration) todo]]
    [methodDeclaration
     [(typeTypeOrVoid identifier formalParameters (* brackets) (? methodDeclaration.5) methodBody) todo]]
    [brackets
     [(LBRACK RBRACK) todo]]
    [methodDeclaration.5
     [(THROWS qualifiedNameList) todo]]
    [methodBody
     [(block) todo]
     [(SEMI) todo]]
    [typeTypeOrVoid
     [(typeType) todo]
     [(VOID) todo]]
    [genericMethodDeclaration
     [(typeParameters methodDeclaration) todo]]
    [genericConstructorDeclaration
     [(typeParameters constructorDeclaration) todo]]
    [constructorDeclaration
     [(identifier formalParameters (? constructorDeclaration.3) block) todo]] ;constructorBody = block
    [constructorDeclaration.3
     [(THROWS qualifiedNameList) todo]]
    [compactConstructorDeclaration
     [((* modifier) identifier block) todo]] ;constructorBody = block
    [fieldDeclaration
     [(typeType variableDeclarators SEMI) todo]]
    [interfaceBodyDeclaration
     [((* modifier) interfaceMemberDeclaration) todo]
     [(SEMI) todo]]
    [interfaceMemberDeclaration
     [(recordDeclaration) todo]
     [(constDeclaration) todo]
     [(interfaceMethodDeclaration) todo]
     [(genericInterfaceMethodDeclaration) todo]
     [(interfaceDeclaration) todo]
     [(annotationTypeDeclaration) todo]
     [(classDeclaration) todo]
     [(enumDeclaration) todo]]
    [constDeclaration
     [(typeType (sep-by COMMA constantDeclarator) SEMI) todo]]
    [constantDeclarator
     [(identifier (* brackets) ASSIGN variableInitializer) todo]]
    [interfaceMethodDeclaration
     [((* interfaceMethodModifier) interfaceCommonBodyDeclaration) todo]]
    [interfaceMethodModifier
     [(annotation) todo]
     [(PUBLIC) todo]
     [(ABSTRACT) todo]
     [(DEFAULT) todo]
     [(STATIC) todo]
     [(STRICTFP) todo]]
    [genericInterfaceMethodDeclaration
     [((* interfaceMethodModifier) typeParameters interfaceCommonBodyDeclaration) todo]]
    [interfaceCommonBodyDeclaration
     [(annotation* typeTypeOrVoid identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody) todo]]
    [interfaceCommonBodyDeclaration.6
     [(THROWS qualifiedNameList) todo]]
    [variableDeclarators
     [((sep-by COMMA variableDeclarator)) todo]]
    [variableDeclarator
     [(variableDeclaratorId (? variableDeclarator.2)) todo]]
    [variableDeclarator.2
     [(ASSIGN variableInitializer) todo]]
    [variableDeclaratorId
     [(identifier (* brackets)) todo]]
    [variableInitializer
     [(arrayInitializer) todo]
     [(expression) todo]]
    [arrayInitializer
     [(LBRACE (? arrayInitializer.2) RBRACE) todo]]
    [arrayInitializer.2
     [((sep-by COMMA variableInitializer) (? COMMA)) todo]]
    ;----
    [variableDeclaratorId ;257
     [(identifier (* variableDeclaratorId.2)) todo]]
    [variableDeclaratorId.2
     [(LBRACK RBRACK) todo]]
    [packageName ;276
     [(identifier (* packageName.2)) (cons $1 $2)]]
    [packageName.2
     [(DOT identifier) $2]]
    [qualifiedNameList ;285
     [(qualifiedName (* qualifiedNameList.2)) (cons $1 $2)]]
    [qualifiedNameList.2
     [(COMMA qualifiedName) $2]]
    [qualifiedName ;316
     [(identifier (* qualifiedName.2)) (cons $1 $2)]]
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