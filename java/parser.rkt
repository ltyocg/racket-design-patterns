#lang racket/base
(require parser-tools/lex
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
   [expected-SR-conflicts 2149]
   [expected-RR-conflicts 585]
   [grammar
    [compilationUnit
     [((* annotation) compilationUnit.1) (ast-compilation-unit (list $1 $2))]
     [(modularCompulationUnit) (ast-compilation-unit (list $1))]]
    [compilationUnit.1
     [(PACKAGE qualifiedName SEMI (* compilationUnit.2) (* compilationUnit.3)) (ast-compilation-unit-body (list 'PACKAGE $2 'SEMI $4 $5))]
     [((* compilationUnit.2) (* compilationUnit.3)) (ast-compilation-unit-body (list $1 $2))]]
    [compilationUnit.2
     [(importDeclaration) (ast-compilation-unit-import (list $1))]
     [(SEMI) '()]]
    [compilationUnit.3
     [(typeDeclaration) (ast-compilation-unit-type (list $1))]
     [(SEMI) '()]]
    [modularCompulationUnit
     [((* importDeclaration) moduleDeclaration) (ast-modular-compilation-unit (list $1 $2))]]
    [packageDeclaration
     [((* annotation) PACKAGE qualifiedName SEMI) (ast-package-declaration (list $1 'PACKAGE $3 'SEMI))]]
    [importDeclaration
     [(IMPORT (?->bool STATIC) qualifiedName importDeclaration.4 SEMI) (ast-import-declaration (list 'IMPORT $2 $3 $4 'SEMI))]]
    [importDeclaration.4
     [(DOT MUL) #t]
     [() #f]]
    [typeDeclaration
     [((* annotation) (* typeDeclarationModifier) typeDeclaration.2) (ast-type-declaration (list $1 $2 $3))]]
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
     [(VOLATILE) 'volatile]
     [(DEFAULT) 'default]]
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
     [(LT (sep-by COMMA typeParameter) GT) $2]]
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
     [(LBRACK RBRACK) (ast-brackets (list 'LBRACK 'RBRACK))]]
    [methodDeclaration.5
     [(THROWS qualifiedNameList) (ast-method-declaration-throws (list 'THROWS $2))]]
    [methodBody
     [(block) (ast-method-body (list $1))]
     [(SEMI) (ast-method-body (list 'SEMI))]]
    [typeTypeOrVoid
     [(typeType) (ast-type-type-or-void (list $1))]
     [(VOID) (ast-type-type-or-void (list 'VOID))]]
    [genericMethodDeclaration
     [(typeParameters memberCommonDeclaration) (ast-generic-method-declaration (list $1 $2))]]
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
     [(identifier (* brackets)) (ast-variable-declarator-id (list $1 $2))]]
    [variableInitializer
     [(arrayInitializer) (ast-variable-initializer (list $1))]
     [(expression) (ast-variable-initializer (list $1))]]
    [arrayInitializer
     [(LBRACE (? arrayInitializer.2) RBRACE) (ast-array-initializer (list 'LBRACE $2 'RBRACE))]]
    [arrayInitializer.2
     [((sep-by COMMA variableInitializer) (?->bool COMMA)) (ast-array-initializer-elements (list $1 $2))]]
    [classType
     [(identifier (? typeArguments) (* classType.2)) (ast-class-type (list $1 $2 $3))]]
    [classType.1
     [((? classType.1.1) typeIdentifier (? typeArguments)) (ast-class-type-segment (list $1 $2 $3))]]
    [classType.1.1
     [(packageName DOT (* annotation)) (ast-class-type-package-prefix (list $1 'DOT $3))]]
    [classType.2
     [(DOT (* annotation) identifier (? typeArguments)) (ast-class-type-suffix (list 'DOT $2 $3 $4))]]
    [packageName
     [((sep-by DOT identifier)) $1]]
    [typeArgument
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
     [(receiverParameter) (ast-formal-parameters-first (list $1))]
     [(formalParameter) (ast-formal-parameters-first (list $1))]]
    [formalParameters.2.2
     [(COMMA formalParameterList) $2]]
    [receiverParameter
     [(typeType (* receiverParameter.2) THIS) (ast-receiver-parameter (list $1 $2 'THIS))]]
    [receiverParameter.2
     [(identifier DOT) (ast-receiver-parameter-qualifier (list $1 'DOT))]]
    [formalParameterList
     [((sep-by COMMA formalParameter)) $1]]
    [formalParameter
     [(identifier (? typeArguments) (* classType.2) (* typeType.3) (? formalParameter.3) variableDeclaratorId)
      (ast-formal-parameter (list
            '()
            (ast-type-type (list
                  '()
                  (ast-type-base (list
                        (ast-class-or-interface-type (list
                              (ast-class-type (list $1 $2 $3))))))
                  $4))
            $5
            $6))]
     [(primitiveType (* typeType.3) (? formalParameter.3) variableDeclaratorId)
      (ast-formal-parameter (list
            '()
            (ast-type-type (list '() (ast-type-base (list $1)) $2))
            $3
            $4))]
     [((* variableModifier) typeType (? formalParameter.3) variableDeclaratorId) (ast-formal-parameter (list $1 $2 $3 $4))]]
    [lambdaLVTIList
     [((sep-by COMMA lambdaLVTIParameter)) $1]]
    [lambdaLVTIParameter
     [((* variableModifier) VAR identifier) (ast-lambda-lvti-parameter (list $1 'VAR $3))]]
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
     [(classBodyDeclaration) $1]
     [(compactConstructorDeclaration) $1]]
    [block
     [(LBRACE (* blockItem) RBRACE) (ast-block (list $2))]]
    [blockItem
     [(block) $1]
     [(ABSTRACT) 'ABSTRACT]
     [(ASSERT) 'ASSERT]
     [(BOOLEAN) 'BOOLEAN]
     [(BREAK) 'BREAK]
     [(BYTE) 'BYTE]
     [(CASE) 'CASE]
     [(CATCH) 'CATCH]
     [(CHAR) 'CHAR]
     [(CLASS) 'CLASS]
     [(CONST) 'CONST]
     [(CONTINUE) 'CONTINUE]
     [(DEFAULT) 'DEFAULT]
     [(DO) 'DO]
     [(DOUBLE) 'DOUBLE]
     [(ELSE) 'ELSE]
     [(ENUM) 'ENUM]
     [(EXPORTS) 'EXPORTS]
     [(EXTENDS) 'EXTENDS]
     [(FINAL) 'FINAL]
     [(FINALLY) 'FINALLY]
     [(FLOAT) 'FLOAT]
     [(FOR) 'FOR]
     [(GOTO) 'GOTO]
     [(IF) 'IF]
     [(IMPLEMENTS) 'IMPLEMENTS]
     [(IMPORT) 'IMPORT]
     [(INSTANCEOF) 'INSTANCEOF]
     [(INT) 'INT]
     [(INTERFACE) 'INTERFACE]
     [(LONG) 'LONG]
     [(MODULE) 'MODULE]
     [(NATIVE) 'NATIVE]
     [(NEW) 'NEW]
     [(NON_SEALED) 'NON_SEALED]
     [(OPEN) 'OPEN]
     [(OPENS) 'OPENS]
     [(PACKAGE) 'PACKAGE]
     [(PERMITS) 'PERMITS]
     [(PRIVATE) 'PRIVATE]
     [(PROTECTED) 'PROTECTED]
     [(PROVIDES) 'PROVIDES]
     [(PUBLIC) 'PUBLIC]
     [(RECORD) 'RECORD]
     [(REQUIRES) 'REQUIRES]
     [(RETURN) 'RETURN]
     [(SEALED) 'SEALED]
     [(SHORT) 'SHORT]
     [(STATIC) 'STATIC]
     [(STRICTFP) 'STRICTFP]
     [(SUPER) 'SUPER]
     [(SWITCH) 'SWITCH]
     [(SYNCHRONIZED) 'SYNCHRONIZED]
     [(THIS) 'THIS]
     [(THROW) 'THROW]
     [(THROWS) 'THROWS]
     [(TO) 'TO]
     [(TRANSIENT) 'TRANSIENT]
     [(TRANSITIVE) 'TRANSITIVE]
     [(TRY) 'TRY]
     [(USES) 'USES]
     [(VAR) 'VAR]
     [(VOID) 'VOID]
     [(VOLATILE) 'VOLATILE]
     [(WHEN) 'WHEN]
     [(WHILE) 'WHILE]
     [(WITH) 'WITH]
     [(YIELD) 'YIELD]
     [(NULL_LITERAL) 'NULL_LITERAL]
     [(LPAREN) 'LPAREN]
     [(RPAREN) 'RPAREN]
     [(LBRACK) 'LBRACK]
     [(RBRACK) 'RBRACK]
     [(SEMI) 'SEMI]
     [(COMMA) 'COMMA]
     [(DOT) 'DOT]
     [(ASSIGN) 'ASSIGN]
     [(GT) 'GT]
     [(LT) 'LT]
     [(BANG) 'BANG]
     [(TILDE) 'TILDE]
     [(QUESTION) 'QUESTION]
     [(COLON) 'COLON]
     [(EQUAL) 'EQUAL]
     [(LE) 'LE]
     [(GE) 'GE]
     [(NOTEQUAL) 'NOTEQUAL]
     [(AND) 'AND]
     [(OR) 'OR]
     [(INC) 'INC]
     [(DEC) 'DEC]
     [(ADD) 'ADD]
     [(SUB) 'SUB]
     [(MUL) 'MUL]
     [(DIV) 'DIV]
     [(BITAND) 'BITAND]
     [(BITOR) 'BITOR]
     [(CARET) 'CARET]
     [(MOD) 'MOD]
     [(ADD_ASSIGN) 'ADD_ASSIGN]
     [(SUB_ASSIGN) 'SUB_ASSIGN]
     [(MUL_ASSIGN) 'MUL_ASSIGN]
     [(DIV_ASSIGN) 'DIV_ASSIGN]
     [(AND_ASSIGN) 'AND_ASSIGN]
     [(OR_ASSIGN) 'OR_ASSIGN]
     [(XOR_ASSIGN) 'XOR_ASSIGN]
     [(MOD_ASSIGN) 'MOD_ASSIGN]
     [(LSHIFT_ASSIGN) 'LSHIFT_ASSIGN]
     [(RSHIFT_ASSIGN) 'RSHIFT_ASSIGN]
     [(URSHIFT_ASSIGN) 'URSHIFT_ASSIGN]
     [(ARROW) 'ARROW]
     [(COLONCOLON) 'COLONCOLON]
     [(AT) 'AT]
     [(AT_INTERFACE) 'AT_INTERFACE]
     [(ELLIPSIS) 'ELLIPSIS]
     [(DECIMAL_LITERAL) $1]
     [(HEX_LITERAL) $1]
     [(OCT_LITERAL) $1]
     [(BINARY_LITERAL) $1]
     [(FLOAT_LITERAL) $1]
     [(HEX_FLOAT_LITERAL) $1]
     [(BOOL_LITERAL) $1]
     [(CHAR_LITERAL) $1]
     [(STRING_LITERAL) $1]
     [(TEXT_BLOCK) $1]
     [(IDENTIFIER) $1]]
    [blockStatement
     [(VAR identifier ASSIGN expression SEMI)
      (ast-block-statement (list
            (ast-local-variable-declaration (list
                  '()
                  (ast-local-variable-declaration-rest (list 'VAR $2 'ASSIGN $4))))))]
     [(primitiveType variableDeclarators SEMI)
      (ast-block-statement (list
            (ast-local-variable-declaration (list
                  '()
                  (ast-local-variable-declaration-rest (list
                        (ast-type-type (list
                              '()
                              (ast-type-base (list $1))
                              '()))
                        $2))))))]
     [(identifier (? typeArguments) (* classType.2) (* typeType.3) variableDeclarators SEMI)
      (ast-block-statement (list
            (ast-local-variable-declaration (list
                  '()
                  (ast-local-variable-declaration-rest (list
                        (ast-type-type (list
                              '()
                              (ast-type-base (list
                                    (ast-class-or-interface-type (list
                                          (ast-class-type (list $1 $2 $3))))))
                              $4))
                        $5))))))]
     [(localVariableDeclaration SEMI) $1]
     [(localTypeDeclaration) $1]
     [(statement) $1]]
    [localVariableDeclaration
     [((* variableModifier) localVariableDeclaration.2) (ast-local-variable-declaration (list $1 $2))]]
    [localVariableDeclaration.2
     [(VAR identifier ASSIGN expression) (ast-local-variable-declaration-rest (list 'VAR $2 'ASSIGN $4))]
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
     [(YIELD (? expression) SEMI) (ast-statement (list 'YIELD $2 'SEMI))]
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
     [(CATCH LPAREN (* variableModifier) catchType identifier RPAREN block) (ast-catch-clause (list 'CATCH 'LPAREN $3 $4 $5 'RPAREN $7))]]
    [catchType
     [((sep-by BITOR qualifiedName)) $1]]
    [finallyBlock
     [(FINALLY block) (ast-finally-block (list 'FINALLY $2))]]
    [resourceSpecification
     [(LPAREN resources (?->bool SEMI) RPAREN) (ast-resource-specification (list 'LPAREN $2 $3 'RPAREN))]]
    [resources
     [((sep-by SEMI resource)) $1]]
    [resource
     [((* variableModifier) classOrInterfaceType variableDeclaratorId ASSIGN expression) (ast-resource (list $1 $2 $3 'ASSIGN $5))]
     [((* variableModifier) VAR identifier ASSIGN expression) (ast-resource (list $1 'VAR $3 'ASSIGN $5))]
     [(qualifiedName) (ast-resource (list $1))]]
    [switchBlockStatementGroup
     [((+ switchBlockStatementGroup.1) (+ blockStatement)) (ast-switch-block-statement-group (list $1 $2))]]
    [switchBlockStatementGroup.1
     [(switchLabel COLON) (ast-switch-block-statement-group-label (list $1 'COLON))]]
    [switchLabel
     [(CASE expression) (ast-switch-label (list 'CASE $2))]
     [(CASE IDENTIFIER) (ast-switch-label (list 'CASE $2))]
     [(CASE typeType identifier) (ast-switch-label (list 'CASE $2 $3))]
     [(DEFAULT) 'default]]
    [forControl
     [(enhancedForControl) (ast-for-control (list $1))]
     [((? forInit) SEMI (? expression) SEMI (? expressionList)) (ast-for-control (list $1 'SEMI $3 'SEMI $5))]]
    [forInit
     [(localVariableDeclaration) (ast-for-init (list $1))]
     [(expressionList) (ast-for-init (list $1))]]
    [enhancedForControl
     [((* variableModifier) typeType variableDeclaratorId COLON expression) (ast-enhanced-for-control (list $1 $2 $3 'COLON $5))]
     [((* variableModifier) VAR variableDeclaratorId COLON expression) (ast-enhanced-for-control (list $1 'VAR $3 'COLON $5))]]
    [expressionList
     [((sep-by COMMA expression)) $1]]
    [methodCall
     [(identifier arguments) (ast-method-call (list $1 $2))]
     [(THIS arguments) (ast-method-call (list 'THIS $2))]
     [(SUPER arguments) (ast-method-call (list 'SUPER $2))]]
    [expression
     [(primary) (ast-expression (list $1))]
     [(expression LBRACK expression RBRACK) (ast-expression (list $1 'LBRACK $3 'RBRACK))]
     [(expression DOT identifier) (ast-expression (list $1 'DOT $3))]
     [(expression DOT methodCall) (ast-expression (list $1 'DOT $3))]
     [(expression DOT THIS) (ast-expression (list $1 'DOT 'THIS))]
     [(expression DOT NEW (? nonWildcardTypeArguments) innerCreator) (ast-expression (list $1 'DOT 'NEW $4 $5))]
     [(expression DOT SUPER superSuffix) (ast-expression (list $1 'DOT 'SUPER $4))]
     [(expression DOT explicitGenericInvocation) (ast-expression (list $1 'DOT $3))]
     [(methodCall) (ast-expression (list $1))]
     [(expression COLONCOLON (? typeArguments) identifier) (ast-expression (list $1 'COLONCOLON $3 $4))]
     [(typeType COLONCOLON (? typeArguments) identifier) (ast-expression (list $1 'COLONCOLON $3 $4))]
     [(typeType COLONCOLON NEW) (ast-expression (list $1 'COLONCOLON 'NEW))]
     [(classType COLONCOLON (? typeArguments) NEW) (ast-expression (list $1 'COLONCOLON $3 'NEW))]
     [(switchExpression) (ast-expression (list $1))]
     [(expression expression.18) (ast-expression (list $1 $2))] ;postfix
     [(expression.20 expression) (ast-expression (list $1 $2))] ;prefix
     [(LPAREN (* annotation) typeType (* expression.23) RPAREN expression) (ast-expression (list 'LPAREN $2 $3 $4 'RPAREN $6))] ;cast
     [(NEW creator) (ast-expression (list 'NEW $2))]
     [(expression expression.26 expression) (ast-expression (list $1 $2 $3))] ;bop */%
     [(expression expression.27 expression) (ast-expression (list $1 $2 $3))] ;bop +-
     [(expression expression.28 expression) (ast-expression (list $1 $2 $3))] ;bop << >> >>>
     [(expression expression.29 expression) (ast-expression (list $1 $2 $3))] ;bop <= >= > <
     [(expression INSTANCEOF typeType) (ast-expression (list $1 'INSTANCEOF $3))]
     [(expression INSTANCEOF pattern) (ast-expression (list $1 'INSTANCEOF $3))]
     [(expression expression.32 expression) (ast-expression (list $1 $2 $3))] ;bop == !=
     [(expression BITAND expression) (ast-expression (list $1 'BITAND $3))]
     [(expression CARET expression) (ast-expression (list $1 'CARET $3))]
     [(expression BITOR expression) (ast-expression (list $1 'BITOR $3))]
     [(expression AND expression) (ast-expression (list $1 'AND $3))]
     [(expression OR expression) (ast-expression (list $1 'OR $3))]
     [(expression QUESTION expression COLON expression) (ast-expression (list $1 'QUESTION $3 'COLON $5))]
     [(expression expression.39 expression) (ast-expression (list $1 $2 $3))] ;bop assignment
     [(lambdaExpression) (ast-expression (list $1))]]
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
     [(LT LT) 'shl]
     [(GT GT GT) 'ushr]
     [(GT GT) 'shr]]
    [expression.29
     [(LE) 'le]
     [(GE) 'ge]
     [(GT) 'gt]
     [(LT) 'lt]]
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
      [((* variableModifier) typeType (* annotation) variableDeclarators) (ast-pattern (list $1 $2 $3 $4))]
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
     [(CASE expressionList switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE $2 $3 $4))]
     [(CASE NULL_LITERAL switchLabeledRule.2.2 switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE 'NULL_LITERAL $3 $4 $5))]
     [(CASE (+ casePattern) (? guard) switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'CASE $2 $3 $4 $5))]
     [(DEFAULT switchLabeledRule.3 switchRuleOutcome) (ast-switch-labeled-rule (list 'DEFAULT $2 $3))]]
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
     [(LT GT) (ast-type-arguments-or-diamond (list 'LT 'GT))]
     [(typeArguments) (ast-type-arguments-or-diamond (list $1))]]
    [nonWildcardTypeArgumentsOrDiamond
     [(LT GT) (ast-non-wildcard-type-arguments-or-diamond (list 'LT 'GT))]
     [(nonWildcardTypeArguments) (ast-non-wildcard-type-arguments-or-diamond (list $1))]]
    [nonWildcardTypeArguments
     [(LT typeList GT) (ast-non-wildcard-type-arguments (list 'LT $2 'GT))]]
    [typeList
     [((sep-by COMMA typeType)) $1]]
    [typeType
     [((* annotation) typeType.2 (* typeType.3)) (ast-type-type (list $1 $2 $3))]]
    [typeType.2
     [(classOrInterfaceType) (ast-type-base (list $1))]
     [(primitiveType) (ast-type-base (list $1))]]
    [typeType.3
     [((* annotation) LBRACK RBRACK) (ast-type-array-suffix (list $1 'LBRACK 'RBRACK))]]
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
     [(LT (sep-by COMMA typeArgument) GT) $2]
     [(LT typeArgumentWithClose) (list $2)]]
    [typeArgumentWithClose
     [(typeType GT) (ast-type-argument-with-close (list $1 'GT))]]
    [superSuffix
     [(arguments) (ast-super-suffix (list $1))]
     [(DOT (? typeArguments) identifier (? arguments)) (ast-super-suffix (list 'DOT $2 $3 $4))]]
    [explicitGenericInvocationSuffix
     [(SUPER superSuffix) (ast-explicit-generic-invocation-suffix (list 'SUPER $2))]
     [(identifier arguments) (ast-explicit-generic-invocation-suffix (list $1 $2))]]
    [arguments
     [(LPAREN (? expressionList) RPAREN) $2]]
    [formalParameter.3
     [((* annotation) ELLIPSIS) (ast-formal-parameter-varargs (list $1 'ELLIPSIS))]]
    [enumDeclaration.3
     [(IMPLEMENTS typeList) (ast-enum-declaration-implements (list 'IMPLEMENTS $2))]]
    [altAnnotationQualifiedName.1
     [(identifier DOT) (ast-alt-annotation-qualified-name-prefix (list $1 'DOT))]]]))

(define (parse-java-code input)
  (define port (open-input-string input))
  (port-count-lines! port)
  (java-parser (lambda () (next-significant-token port))))

(define (parse-java-file path)
  (call-with-input-file path
    (lambda (port)
      (port-count-lines! port)
      (java-parser (lambda () (next-significant-token port))))))
