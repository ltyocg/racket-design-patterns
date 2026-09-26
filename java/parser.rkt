#lang racket/base
(require parser-tools/lex
         "lexer.rkt"
         "ext-parser.rkt"
         "ast.rkt")

(provide java-parser
         parse-java-code
         parse-java-file
         java-node
         java-node?
         java-node-kind
         java-node-children
         node)

(struct java-node (kind children) #:transparent)

(define (node kind . children)
  (java-node kind children))

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
     [((* annotation) compilationUnit.1) (node 'compilationUnit $1 $2)]
     [(modularCompulationUnit) (node 'compilationUnit $1)]]
    [compilationUnit.1
     [(PACKAGE qualifiedName SEMI (* compilationUnit.2) (* compilationUnit.3)) (node 'compilationUnit.1 'PACKAGE $2 'SEMI $4 $5)]
     [((* compilationUnit.2) (* compilationUnit.3)) (node 'compilationUnit.1 $1 $2)]]
    [compilationUnit.2
     [(importDeclaration) (node 'compilationUnit.2 $1)]
     [(SEMI) '()]]
    [compilationUnit.3
     [(typeDeclaration) (node 'compilationUnit.3 $1)]
     [(SEMI) '()]]
    [modularCompulationUnit
     [((* importDeclaration) moduleDeclaration) (node 'modularCompulationUnit $1 $2)]]
    [packageDeclaration
     [((* annotation) PACKAGE qualifiedName SEMI) (node 'packageDeclaration $1 'PACKAGE $3 'SEMI)]]
    [importDeclaration
     [(IMPORT (?->bool STATIC) qualifiedName importDeclaration.4 SEMI) (node 'importDeclaration 'IMPORT $2 $3 $4 'SEMI)]]
    [importDeclaration.4
     [(DOT MUL) #t]
     [() #f]]
    [typeDeclaration
     [((* annotation) (* typeDeclarationModifier) typeDeclaration.2) (node 'typeDeclaration $1 $2 $3)]]
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
     [(classDeclaration) (node 'typeDeclaration.2 $1)]
     [(enumDeclaration) (node 'typeDeclaration.2 $1)]
     [(interfaceDeclaration) (node 'typeDeclaration.2 $1)]
     [(annotationTypeDeclaration) (node 'typeDeclaration.2 $1)]
     [(recordDeclaration) (node 'typeDeclaration.2 $1)]]
    [modifier
     [(classOrInterfaceModifier) (node 'modifier $1)]
     [(NATIVE) 'native]
     [(SYNCHRONIZED) 'synchronized]
     [(TRANSIENT) 'transient]
     [(VOLATILE) 'volatile]
     [(DEFAULT) 'default]]
    [classOrInterfaceModifier
     [(annotation) (node 'classOrInterfaceModifier $1)]
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
     [(annotation) (node 'variableModifier $1)]]
    [classDeclaration
     [(CLASS identifier (? typeParameters) (? classDeclaration.4) (? classDeclaration.5) (? classDeclaration.6) classBody) (node 'classDeclaration 'CLASS $2 $3 $4 $5 $6 $7)]]
    [classDeclaration.4
     [(EXTENDS typeType) (node 'classDeclaration.4 'EXTENDS $2)]]
    [classDeclaration.5
     [(IMPLEMENTS typeList) (node 'classDeclaration.5 'IMPLEMENTS $2)]]
    [classDeclaration.6
     [(PERMITS typeList) (node 'classDeclaration.6 'PERMITS $2)]]
    [typeParameters
     [(LT (sep-by COMMA typeParameter) GT) $2]]
    [typeParameter
     [((* annotation) identifier (? typeParameter.3)) (node 'typeParameter $1 $2 $3)]]
    [typeParameter.3
     [(EXTENDS (* annotation) typeBound) (node 'typeParameter.3 'EXTENDS $2 $3)]]
    [typeBound
     [((sep-by BITAND typeType)) $1]]
    [enumDeclaration
     [(ENUM identifier (? enumDeclaration.3) LBRACE (? enumConstants) (?->bool COMMA) (? enumBodyDeclarations) RBRACE) (node 'enumDeclaration 'ENUM $2 $3 'LBRACE $5 $6 $7 'RBRACE)]]
    [enumConstants
     [((sep-by COMMA enumConstant)) (node 'enumConstants $1)]]
    [enumConstant
     [((* annotation) identifier (? arguments) (? classBody)) (node 'enumConstant $1 $2 $3 $4)]]
    [enumBodyDeclarations
     [(SEMI (* classBodyDeclaration)) (node 'enumBodyDeclarations 'SEMI $2)]]
    [interfaceDeclaration
     [(INTERFACE identifier (? typeParameters) (? interfaceDeclaration.4) (? interfaceDeclaration.5) interfaceBody) (node 'interfaceDeclaration 'INTERFACE $2 $3 $4 $5 $6)]]
    [interfaceDeclaration.4
     [(EXTENDS typeList) (node 'interfaceDeclaration.4 'EXTENDS $2)]]
    [interfaceDeclaration.5
     [(PERMITS typeList) (node 'interfaceDeclaration.5 'PERMITS $2)]]
    [classBody
     [(LBRACE (* classBodyDeclaration) RBRACE) (node 'classBody 'LBRACE $2 'RBRACE)]]
    [interfaceBody
     [(LBRACE (* interfaceBodyDeclaration) RBRACE) (node 'interfaceBody 'LBRACE $2 'RBRACE)]]
    [classBodyDeclaration
     [(SEMI) (node 'classBodyDeclaration 'SEMI)]
     [((?->bool STATIC) block) (node 'classBodyDeclaration $1 $2)]
     [((* modifier) memberDeclaration) (node 'classBodyDeclaration $1 $2)]]
    [memberDeclaration
     [(recordDeclaration) (node 'memberDeclaration $1)]
     [(memberCommonDeclaration) (node 'memberDeclaration $1)]
     [(typeParameters memberCommonDeclaration) (node 'genericMemberDeclaration $1 $2)]
     [(interfaceDeclaration) (node 'memberDeclaration $1)]
     [(annotationTypeDeclaration) (node 'memberDeclaration $1)]
     [(classDeclaration) (node 'memberDeclaration $1)]
     [(enumDeclaration) (node 'memberDeclaration $1)]]
    [memberCommonDeclaration
     [(typeTypeOrVoid memberCommonDeclaration.2) (node 'memberCommonDeclaration $1 $2)]]
    [memberCommonDeclaration.2
     [(identifier formalParameters (* brackets) (? methodDeclaration.5) methodBody)
      (node 'methodDeclaration $1 $2 $3 $4 $5)]
     [(formalParameters (? constructorDeclaration.3) block)
      (node 'constructorDeclaration $1 $2 $3)]
     [(identifier (* brackets) (? variableDeclarator.2) (* memberCommonDeclaration.3) SEMI)
      (node 'fieldDeclaration
            (cons (node 'variableDeclarator
                        (node 'variableDeclaratorId $1 $2)
                        $3)
                  $4)
            'SEMI)]]
    [memberCommonDeclaration.3
     [(COMMA variableDeclarator) $2]]
    [methodDeclaration
     [(typeTypeOrVoid identifier formalParameters (* brackets) (? methodDeclaration.5) methodBody) (node 'methodDeclaration $1 $2 $3 $4 $5 $6)]]
    [brackets
     [(LBRACK RBRACK) (node 'brackets 'LBRACK 'RBRACK)]]
    [methodDeclaration.5
     [(THROWS qualifiedNameList) (node 'methodDeclaration.5 'THROWS $2)]]
    [methodBody
     [(block) (node 'methodBody $1)]
     [(SEMI) (node 'methodBody 'SEMI)]]
    [typeTypeOrVoid
     [(typeType) (node 'typeTypeOrVoid $1)]
     [(VOID) (node 'typeTypeOrVoid 'VOID)]]
    [genericMethodDeclaration
     [(typeParameters memberCommonDeclaration) (node 'genericMethodDeclaration $1 $2)]]
    [genericConstructorDeclaration
     [(typeParameters constructorDeclaration) (node 'genericConstructorDeclaration $1 $2)]]
    [constructorDeclaration
     [(identifier formalParameters (? constructorDeclaration.3) block) (node 'constructorDeclaration $1 $2 $3 $4)]] ;constructorBody = block
    [constructorDeclaration.3
     [(THROWS qualifiedNameList) (node 'constructorDeclaration.3 'THROWS $2)]]
    [compactConstructorDeclaration
     [((* modifier) identifier block) (node 'compactConstructorDeclaration $1 $2 $3)]] ;constructorBody = block
    [fieldDeclaration
     [(typeType variableDeclarators SEMI) (node 'fieldDeclaration $1 $2 'SEMI)]]
    [interfaceBodyDeclaration
     [((* modifier) interfaceMemberDeclaration) (node 'interfaceBodyDeclaration $1 $2)]
     [(SEMI) (node 'interfaceBodyDeclaration 'SEMI)]]
    [interfaceMemberDeclaration
     [(recordDeclaration) (node 'interfaceMemberDeclaration $1)]
     [(interfaceCommonMemberDeclaration) (node 'interfaceMemberDeclaration $1)]
     [(typeParameters interfaceCommonMemberDeclaration) (node 'genericInterfaceMemberDeclaration $1 $2)]
     [(interfaceDeclaration) (node 'interfaceMemberDeclaration $1)]
     [(annotationTypeDeclaration) (node 'interfaceMemberDeclaration $1)]
     [(classDeclaration) (node 'interfaceMemberDeclaration $1)]
     [(enumDeclaration) (node 'interfaceMemberDeclaration $1)]]
    [interfaceCommonMemberDeclaration
     [(typeTypeOrVoid interfaceCommonMemberDeclaration.2) (node 'interfaceCommonMemberDeclaration $1 $2)]]
    [interfaceCommonMemberDeclaration.2
     [(identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody)
      (node 'interfaceCommonBodyDeclaration '() $1 $2 $3 $4 $5)]
     [(identifier (* brackets) ASSIGN variableInitializer (* interfaceCommonMemberDeclaration.3) SEMI)
      (node 'constDeclaration
            (cons (node 'constantDeclarator $1 $2 'ASSIGN $4) $5)
            'SEMI)]]
    [interfaceCommonMemberDeclaration.3
     [(COMMA constantDeclarator) $2]]
    [constDeclaration
     [(typeType (sep-by COMMA constantDeclarator) SEMI) (node 'constDeclaration $1 $2 'SEMI)]]
    [constantDeclarator
     [(identifier (* brackets) ASSIGN variableInitializer) (node 'constantDeclarator $1 $2 'ASSIGN $4)]]
    [interfaceMethodDeclaration
     [((* interfaceMethodModifier) interfaceCommonBodyDeclaration) (node 'interfaceMethodDeclaration $1 $2)]]
    [interfaceMethodModifier
     [(annotation) (node 'interfaceMethodModifier $1)]
     [(PUBLIC) (node 'interfaceMethodModifier 'PUBLIC)]
     [(ABSTRACT) (node 'interfaceMethodModifier 'ABSTRACT)]
     [(DEFAULT) (node 'interfaceMethodModifier 'DEFAULT)]
     [(STATIC) (node 'interfaceMethodModifier 'STATIC)]
     [(STRICTFP) (node 'interfaceMethodModifier 'STRICTFP)]]
    [genericInterfaceMethodDeclaration
     [((* interfaceMethodModifier) typeParameters interfaceCommonBodyDeclaration) (node 'genericInterfaceMethodDeclaration $1 $2 $3)]]
    [interfaceCommonBodyDeclaration
     [((* annotation) typeTypeOrVoid identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody) (node 'interfaceCommonBodyDeclaration $1 $2 $3 $4 $5 $6 $7)]]
    [interfaceCommonBodyDeclaration.6
     [(THROWS qualifiedNameList) (node 'interfaceCommonBodyDeclaration.6 'THROWS $2)]]
    [variableDeclarators
     [((sep-by COMMA variableDeclarator)) $1]]
    [variableDeclarator
     [(variableDeclaratorId (? variableDeclarator.2)) (node 'variableDeclarator $1 $2)]]
    [variableDeclarator.2
     [(ASSIGN variableInitializer) (node 'variableDeclarator.2 'ASSIGN $2)]]
    [variableDeclaratorId
     [(identifier (* brackets)) (node 'variableDeclaratorId $1 $2)]]
    [variableInitializer
     [(arrayInitializer) (node 'variableInitializer $1)]
     [(expression) (node 'variableInitializer $1)]]
    [arrayInitializer
     [(LBRACE (? arrayInitializer.2) RBRACE) (node 'arrayInitializer 'LBRACE $2 'RBRACE)]]
    [arrayInitializer.2
     [((sep-by COMMA variableInitializer) (?->bool COMMA)) (node 'arrayInitializer.2 $1 $2)]]
    [classType
     [(identifier (? typeArguments) (* classType.2)) (node 'classType $1 $2 $3)]]
    [classType.1
     [((? classType.1.1) typeIdentifier (? typeArguments)) (node 'classType.1 $1 $2 $3)]]
    [classType.1.1
     [(packageName DOT (* annotation)) (node 'classType.1.1 $1 'DOT $3)]]
    [classType.2
     [(DOT (* annotation) identifier (? typeArguments)) (node 'classType.2 'DOT $2 $3 $4)]]
    [packageName
     [((sep-by DOT identifier)) $1]]
    [typeArgument
     [(typeType) (node 'typeArgument $1)]
     [((* annotation) QUESTION (? typeArgument.3)) (node 'typeArgument $1 'QUESTION $3)]]
    [typeArgument.3
     [(typeArgument.3.1 typeType) (node 'typeArgument.3 $1 $2)]]
    [typeArgument.3.1
     [(EXTENDS) 'extends]
     [(SUPER) 'super]]
    [qualifiedNameList
     [((sep-by COMMA qualifiedName)) $1]]
    [formalParameters
     [(LPAREN (? formalParameters.2) RPAREN) $2]]
    [formalParameters.2
     [(formalParameters.2.1 (* formalParameters.2.2)) (node 'formalParameters.2 $1 $2)]]
    [formalParameters.2.1
     [(receiverParameter) (node 'formalParameters.2.1 $1)]
     [(formalParameter) (node 'formalParameters.2.1 $1)]]
    [formalParameters.2.2
     [(COMMA formalParameterList) $2]]
    [receiverParameter
     [(typeType (* receiverParameter.2) THIS) (node 'receiverParameter $1 $2 'THIS)]]
    [receiverParameter.2
     [(identifier DOT) (node 'receiverParameter.2 $1 'DOT)]]
    [formalParameterList
     [((sep-by COMMA formalParameter)) $1]]
    [formalParameter
     [(identifier (? typeArguments) (* classType.2) (* typeType.3) (? formalParameter.3) variableDeclaratorId)
      (node 'formalParameter
            '()
            (node 'typeType
                  '()
                  (node 'typeType.2
                        (node 'classOrInterfaceType
                              (node 'classType $1 $2 $3)))
                  $4)
            $5
            $6)]
     [(primitiveType (* typeType.3) (? formalParameter.3) variableDeclaratorId)
      (node 'formalParameter
            '()
            (node 'typeType '() (node 'typeType.2 $1) $2)
            $3
            $4)]
     [((* variableModifier) typeType (? formalParameter.3) variableDeclaratorId) (node 'formalParameter $1 $2 $3 $4)]]
    [lambdaLVTIList
     [((sep-by COMMA lambdaLVTIParameter)) $1]]
    [lambdaLVTIParameter
     [((* variableModifier) VAR identifier) (node 'lambdaLVTIParameter $1 'VAR $3)]]
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
     [((* altAnnotationQualifiedName.1) AT identifier) (node 'altAnnotationQualifiedName $1 'AT $3)]]
    [annotation
     [(annotation.1 (? annotationFieldValues)) (node 'annotation $1 $2)]]
    [annotation.1
     [(AT qualifiedName) $2]]
    [annotationFieldValues
     [(LPAREN (? (sep-by COMMA annotationFieldValue)) RPAREN) $2]]
    [annotationFieldValue
     [(identifier ASSIGN annotationValue) (node 'annotationFieldValue $1 'ASSIGN $3)]
     [(annotationValue) (node 'annotationFieldValue $1)]]
    [annotationValue
     [(expression) (node 'annotationValue $1)]
     [(annotation) (node 'annotationValue $1)]
     [(LBRACE (? (sep-by COMMA annotationValue)) (?->bool COMMA) RBRACE) $2]]
    [elementValue
     [(expression) (node 'elementValue $1)]
     [(annotation) (node 'elementValue $1)]
     [(elementValueArrayInitializer) (node 'elementValue $1)]]
    [elementValueArrayInitializer
     [(LBRACE (? (sep-by COMMA elementValue)) (?->bool COMMA) RBRACE) (node 'elementValueArrayInitializer 'LBRACE $2 $3 'RBRACE)]]
    [annotationTypeDeclaration
     [(AT_INTERFACE identifier annotationTypeBody) (node 'annotationTypeDeclaration 'AT_INTERFACE $2 $3)]]
    [annotationTypeBody
     [(LBRACE (* annotationTypeElementDeclaration) RBRACE) (node 'annotationTypeBody 'LBRACE $2 'RBRACE)]]
    [annotationTypeElementDeclaration
     [((* modifier) annotationTypeElementRest) (node 'annotationTypeElementDeclaration $1 $2)]
     [(SEMI) null]]
    [annotationTypeElementRest
     [(typeType annotationMethodOrConstantRest SEMI) (node 'annotationTypeElementRest $1 $2 'SEMI)]
     [(classDeclaration (?->bool SEMI)) $1]
     [(interfaceDeclaration (?->bool SEMI)) $1]
     [(enumDeclaration (?->bool SEMI)) $1]
     [(annotationTypeDeclaration (?->bool SEMI)) $1]
     [(recordDeclaration (?->bool SEMI)) $1]]
    [annotationMethodOrConstantRest
     [(annotationMethodRest) $1]
     [(annotationConstantRest) $1]]
    [annotationMethodRest
     [(identifier LPAREN RPAREN (? defaultValue)) (node 'annotationMethodRest $1 'LPAREN 'RPAREN $4)]]
    [annotationConstantRest
     [(variableDeclarators) (node 'annotationConstantRest $1)]]
    [defaultValue
      [(DEFAULT elementValue) (node 'defaultValue 'DEFAULT $2)]]
    [moduleDeclaration
     [((* annotation) (?->bool OPEN) MODULE qualifiedName LBRACE (* moduleDirective) RBRACE) (node 'moduleDeclaration $1 $2 'MODULE $4 'LBRACE $6 'RBRACE)]]
    [moduleDirective
     [(REQUIRES (* requiresModifier) qualifiedName SEMI) (node 'moduleDirective 'REQUIRES $2 $3 'SEMI)]
     [(EXPORTS qualifiedName (? moduleDirective.3) SEMI) (node 'moduleDirective 'EXPORTS $2 $3 'SEMI)]
     [(OPENS qualifiedName (? moduleDirective.3) SEMI) (node 'moduleDirective 'OPENS $2 $3 'SEMI)]
     [(USES qualifiedName SEMI) (node 'moduleDirective 'USES $2 'SEMI)]
     [(PROVIDES qualifiedName WITH (sep-by COMMA qualifiedName) SEMI) (node 'moduleDirective 'PROVIDES $2 'WITH $4 'SEMI)]]
    [moduleDirective.3
     [(TO (sep-by COMMA qualifiedName)) (node 'moduleDirective.3 'TO $2)]]
    [requiresModifier
     [(TRANSITIVE) 'transitive]
     [(STATIC) 'static]]
    [recordDeclaration
     [(RECORD identifier (? typeParameters) recordHeader (? recordDeclaration.5) recordBody) (node 'recordDeclaration 'RECORD $2 $3 $4 $5 $6)]]
    [recordDeclaration.5
     [(IMPLEMENTS typeList) $2]]
    [recordHeader
     [(LPAREN (? recordComponentList) RPAREN) $2]]
    [recordComponentList
     [((sep-by COMMA recordComponent)) $1]]
    [recordComponent
     [((* annotation) typeType (? recordComponent.3) identifier) (node 'recordComponent $1 $2 $3 $4)]]
    [recordComponent.3
     [((* annotation) ELLIPSIS) (node 'recordComponent.3 $1 'ELLIPSIS)]]
    [recordBody
     [(LBRACE (* recordBody.2) RBRACE) (node 'recordBody 'LBRACE $2 'RBRACE)]]
    [recordBody.2
     [(classBodyDeclaration) $1]
     [(compactConstructorDeclaration) $1]]
    [block
     [(LBRACE (* blockItem) RBRACE) (node 'block $2)]]
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
      (node 'blockStatement
            (node 'localVariableDeclaration
                  '()
                  (node 'localVariableDeclaration.2 'VAR $2 'ASSIGN $4)))]
     [(primitiveType variableDeclarators SEMI)
      (node 'blockStatement
            (node 'localVariableDeclaration
                  '()
                  (node 'localVariableDeclaration.2
                        (node 'typeType
                              '()
                              (node 'typeType.2 $1)
                              '())
                        $2)))]
     [(identifier (? typeArguments) (* classType.2) (* typeType.3) variableDeclarators SEMI)
      (node 'blockStatement
            (node 'localVariableDeclaration
                  '()
                  (node 'localVariableDeclaration.2
                        (node 'typeType
                              '()
                              (node 'typeType.2
                                    (node 'classOrInterfaceType
                                          (node 'classType $1 $2 $3)))
                              $4)
                        $5)))]
     [(localVariableDeclaration SEMI) $1]
     [(localTypeDeclaration) $1]
     [(statement) $1]]
    [localVariableDeclaration
     [((* variableModifier) localVariableDeclaration.2) (node 'localVariableDeclaration $1 $2)]]
    [localVariableDeclaration.2
     [(VAR identifier ASSIGN expression) (node 'localVariableDeclaration.2 'VAR $2 'ASSIGN $4)]
     [(typeType variableDeclarators) (node 'localVariableDeclaration.2 $1 $2)]]
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
     [((* classOrInterfaceModifier) localTypeDeclaration.2) (node 'localTypeDeclaration $1 $2)]]
    [localTypeDeclaration.2
     [(classDeclaration) $1]
     [(interfaceDeclaration) $1]
     [(recordDeclaration) $1]
     [(enumDeclaration) $1]]
    [statement
     [(block) $1] ;blockLabel = block
     [(ASSERT expression (? statement.3/2) SEMI) (node 'statement 'ASSERT $2 $3 'SEMI)]
     [(IF LPAREN expression RPAREN statement (? statement.6)) (node 'statement 'IF 'LPAREN $3 'RPAREN $5 $6)]
     [(FOR LPAREN forControl RPAREN statement) (node 'statement 'FOR 'LPAREN $3 'RPAREN $5)]
     [(WHILE LPAREN expression RPAREN statement) (node 'statement 'WHILE 'LPAREN $3 'RPAREN $5)]
     [(DO statement WHILE LPAREN expression RPAREN SEMI) (node 'statement 'DO $2 'WHILE 'LPAREN $5 'RPAREN 'SEMI)]
     [(TRY block statement.3/7) (node 'statement 'TRY $2 $3)]
     [(TRY resourceSpecification block (* catchClause) (? finallyBlock)) (node 'statement 'TRY $2 $3 $4 $5)]
     [(SWITCH LPAREN expression RPAREN LBRACE (* switchBlockStatementGroup) (* switchLabel) RBRACE) (node 'statement 'SWITCH 'LPAREN $3 'RPAREN 'LBRACE $6 $7 'RBRACE)]
     [(SYNCHRONIZED LPAREN expression RPAREN block) (node 'statement 'SYNCHRONIZED 'LPAREN $3 'RPAREN $5)]
     [(RETURN (? expression) SEMI) (node 'statement 'RETURN $2 'SEMI)]
     [(THROW expression SEMI) (node 'statement 'THROW $2 'SEMI)]
     [(BREAK (? identifier) SEMI) (node 'statement 'BREAK $2 'SEMI)]
     [(CONTINUE (? identifier) SEMI) (node 'statement 'CONTINUE $2 'SEMI)]
     [(YIELD (? expression) SEMI) (node 'statement 'YIELD $2 'SEMI)]
     [(SEMI) null]
     [(expression SEMI) (node 'statement $1 'SEMI)] ;statementExpression = expression
     [(switchExpression (?->bool SEMI)) (node 'statement $1 $2)]
     [(identifier COLON statement) (node 'statement $1 'COLON $3)]] ;identifierLabel = identifier
    [statement.3/2
     [(COLON expression) $2]]
    [statement.6
     [(ELSE statement) $2]]
    [statement.3/7
     [((+ catchClause) (? finallyBlock)) (node 'statement.3/7 $1 $2)]
     [(finallyBlock) (node 'statement.3/7 $1)]]
    [catchClause
     [(CATCH LPAREN (* variableModifier) catchType identifier RPAREN block) (node 'catchClause 'CATCH 'LPAREN $3 $4 $5 'RPAREN $7)]]
    [catchType
     [((sep-by BITOR qualifiedName)) $1]]
    [finallyBlock
     [(FINALLY block) (node 'finallyBlock 'FINALLY $2)]]
    [resourceSpecification
     [(LPAREN resources (?->bool SEMI) RPAREN) (node 'resourceSpecification 'LPAREN $2 $3 'RPAREN)]]
    [resources
     [((sep-by SEMI resource)) $1]]
    [resource
     [((* variableModifier) classOrInterfaceType variableDeclaratorId ASSIGN expression) (node 'resource $1 $2 $3 'ASSIGN $5)]
     [((* variableModifier) VAR identifier ASSIGN expression) (node 'resource $1 'VAR $3 'ASSIGN $5)]
     [(qualifiedName) (node 'resource $1)]]
    [switchBlockStatementGroup
     [((+ switchBlockStatementGroup.1) (+ blockStatement)) (node 'switchBlockStatementGroup $1 $2)]]
    [switchBlockStatementGroup.1
     [(switchLabel COLON) (node 'switchBlockStatementGroup.1 $1 'COLON)]]
    [switchLabel
     [(CASE expression) (node 'switchLabel 'CASE $2)]
     [(CASE IDENTIFIER) (node 'switchLabel 'CASE $2)]
     [(CASE typeType identifier) (node 'switchLabel 'CASE $2 $3)]
     [(DEFAULT) 'default]]
    [forControl
     [(enhancedForControl) (node 'forControl $1)]
     [((? forInit) SEMI (? expression) SEMI (? expressionList)) (node 'forControl $1 'SEMI $3 'SEMI $5)]]
    [forInit
     [(localVariableDeclaration) (node 'forInit $1)]
     [(expressionList) (node 'forInit $1)]]
    [enhancedForControl
     [((* variableModifier) typeType variableDeclaratorId COLON expression) (node 'enhancedForControl $1 $2 $3 'COLON $5)]
     [((* variableModifier) VAR variableDeclaratorId COLON expression) (node 'enhancedForControl $1 'VAR $3 'COLON $5)]]
    [expressionList
     [((sep-by COMMA expression)) $1]]
    [methodCall
     [(identifier arguments) (node 'methodCall $1 $2)]
     [(THIS arguments) (node 'methodCall 'THIS $2)]
     [(SUPER arguments) (node 'methodCall 'SUPER $2)]]
    [expression
     [(primary) (node 'expression $1)]
     [(expression LBRACK expression RBRACK) (node 'expression $1 'LBRACK $3 'RBRACK)]
     [(expression DOT identifier) (node 'expression $1 'DOT $3)]
     [(expression DOT methodCall) (node 'expression $1 'DOT $3)]
     [(expression DOT THIS) (node 'expression $1 'DOT 'THIS)]
     [(expression DOT NEW (? nonWildcardTypeArguments) innerCreator) (node 'expression $1 'DOT 'NEW $4 $5)]
     [(expression DOT SUPER superSuffix) (node 'expression $1 'DOT 'SUPER $4)]
     [(expression DOT explicitGenericInvocation) (node 'expression $1 'DOT $3)]
     [(methodCall) (node 'expression $1)]
     [(expression COLONCOLON (? typeArguments) identifier) (node 'expression $1 'COLONCOLON $3 $4)]
     [(typeType COLONCOLON (? typeArguments) identifier) (node 'expression $1 'COLONCOLON $3 $4)]
     [(typeType COLONCOLON NEW) (node 'expression $1 'COLONCOLON 'NEW)]
     [(classType COLONCOLON (? typeArguments) NEW) (node 'expression $1 'COLONCOLON $3 'NEW)]
     [(switchExpression) (node 'expression $1)]
     [(expression expression.18) (node 'expression $1 $2)] ;postfix
     [(expression.20 expression) (node 'expression $1 $2)] ;prefix
     [(LPAREN (* annotation) typeType (* expression.23) RPAREN expression) (node 'expression 'LPAREN $2 $3 $4 'RPAREN $6)] ;cast
     [(NEW creator) (node 'expression 'NEW $2)]
     [(expression expression.26 expression) (node 'expression $1 $2 $3)] ;bop */%
     [(expression expression.27 expression) (node 'expression $1 $2 $3)] ;bop +-
     [(expression expression.28 expression) (node 'expression $1 $2 $3)] ;bop << >> >>>
     [(expression expression.29 expression) (node 'expression $1 $2 $3)] ;bop <= >= > <
     [(expression INSTANCEOF typeType) (node 'expression $1 'INSTANCEOF $3)]
     [(expression INSTANCEOF pattern) (node 'expression $1 'INSTANCEOF $3)]
     [(expression expression.32 expression) (node 'expression $1 $2 $3)] ;bop == !=
     [(expression BITAND expression) (node 'expression $1 'BITAND $3)]
     [(expression CARET expression) (node 'expression $1 'CARET $3)]
     [(expression BITOR expression) (node 'expression $1 'BITOR $3)]
     [(expression AND expression) (node 'expression $1 'AND $3)]
     [(expression OR expression) (node 'expression $1 'OR $3)]
     [(expression QUESTION expression COLON expression) (node 'expression $1 'QUESTION $3 'COLON $5)]
     [(expression expression.39 expression) (node 'expression $1 $2 $3)] ;bop assignment
     [(lambdaExpression) (node 'expression $1)]]
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
     [(BITAND typeType) (node 'expression.23 'BITAND $2)]]
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
      [((* variableModifier) typeType (* annotation) variableDeclarators) (node 'pattern $1 $2 $3 $4)]
      [(typeType LPAREN (? componentPatternList) RPAREN) (node 'pattern $1 'LPAREN $3 'RPAREN)]]
    [componentPatternList
     [((sep-by COMMA componentPattern)) $1]]
    [componentPattern
     [(pattern) (node 'componentPattern $1)]]
    [lambdaExpression
     [(lambdaParameters ARROW lambdaBody) (node 'lambdaExpression $1 'ARROW $3)]]
    [lambdaParameters
     [(identifier) (node 'lambdaParameters $1)]
     [(LPAREN (? formalParameterList) RPAREN) (node 'lambdaParameters 'LPAREN $2 'RPAREN)]
     [(LPAREN (sep-by COMMA identifier) RPAREN) (node 'lambdaParameters 'LPAREN $2 'RPAREN)]
     [(LPAREN (? lambdaLVTIList) RPAREN) (node 'lambdaParameters 'LPAREN $2 'RPAREN)]]
    [lambdaBody
     [(expression) (node 'lambdaBody $1)]
     [(block) (node 'lambdaBody $1)]]
    [primary
     [(LPAREN expression RPAREN) (node 'primary 'LPAREN $2 'RPAREN)]
     [(THIS) 'this]
     [(SUPER) 'super]
     [(literal) (node 'primary $1)]
     [(identifier) (node 'primary $1)]
     [(typeTypeOrVoid DOT CLASS) (node 'primary $1 'DOT 'CLASS)]
     [(nonWildcardTypeArguments primary.7) (node 'primary $1 $2)]]
    [primary.7
     [(explicitGenericInvocationSuffix) (node 'primary.7 $1)]
     [(THIS arguments) (node 'primary.7 'THIS $2)]]
    [switchExpression
     [(SWITCH LPAREN expression RPAREN LBRACE (* switchLabeledRule) RBRACE) (node 'switchExpression 'SWITCH 'LPAREN $3 'RPAREN 'LBRACE $6 'RBRACE)]]
    [switchLabeledRule
     [(CASE expressionList switchLabeledRule.3 switchRuleOutcome) (node 'switchLabeledRule 'CASE $2 $3 $4)]
     [(CASE NULL_LITERAL switchLabeledRule.2.2 switchLabeledRule.3 switchRuleOutcome) (node 'switchLabeledRule 'CASE 'NULL_LITERAL $3 $4 $5)]
     [(CASE (+ casePattern) (? guard) switchLabeledRule.3 switchRuleOutcome) (node 'switchLabeledRule 'CASE $2 $3 $4 $5)]
     [(DEFAULT switchLabeledRule.3 switchRuleOutcome) (node 'switchLabeledRule 'DEFAULT $2 $3)]]
    [switchLabeledRule.2.2
     [() #f]
     [(COMMA DEFAULT) #t]]
    [switchLabeledRule.3
     [(ARROW) 'arrow]
     [(COLON) 'colon]]
    [guard
     [(WHEN expression) (node 'guard 'WHEN $2)]]
    [casePattern
     [(pattern) (node 'casePattern $1)]]
    [switchRuleOutcome
     [(block) (node 'switchRuleOutcome $1)]
     [((* blockStatement)) (node 'switchRuleOutcome $1)]]
    [classOrInterfaceType
     [(classType) (node 'classOrInterfaceType $1)]]
    [creator
     [(createdName creatorRest) (node 'creator $1 $2)]
     [(nonWildcardTypeArguments createdName classCreatorRest) (node 'creator $1 $2 $3)]]
    [creatorRest
     [(classCreatorRest) (node 'creatorRest $1)]
     [(arrayCreatorRest) (node 'creatorRest $1)]]
    [createdName
     [(identifier (? typeArgumentsOrDiamond) (* createdName.3)) (node 'createdName $1 $2 $3)]
     [(primitiveType) (node 'createdName $1)]]
    [createdName.3
     [(DOT identifier (? typeArgumentsOrDiamond)) (node 'createdName.3 'DOT $2 $3)]]
    [innerCreator
     [(identifier (? nonWildcardTypeArgumentsOrDiamond) classCreatorRest) (node 'innerCreator $1 $2 $3)]]
    [arrayCreatorRest
     [((+ brackets) arrayInitializer) (node 'arrayCreatorRest $1 $2)]
     [((+ arrayCreatorRest.2) (* brackets)) (node 'arrayCreatorRest $1 $2)]]
    [arrayCreatorRest.2
     [(LBRACK expression RBRACK) (node 'arrayCreatorRest.2 'LBRACK $2 'RBRACK)]]
    [classCreatorRest
     [(arguments (? classBody)) (node 'classCreatorRest $1 $2)]]
    [explicitGenericInvocation
     [(nonWildcardTypeArguments explicitGenericInvocationSuffix) (node 'explicitGenericInvocation $1 $2)]]
    [typeArgumentsOrDiamond
     [(LT GT) (node 'typeArgumentsOrDiamond 'LT 'GT)]
     [(typeArguments) (node 'typeArgumentsOrDiamond $1)]]
    [nonWildcardTypeArgumentsOrDiamond
     [(LT GT) (node 'nonWildcardTypeArgumentsOrDiamond 'LT 'GT)]
     [(nonWildcardTypeArguments) (node 'nonWildcardTypeArgumentsOrDiamond $1)]]
    [nonWildcardTypeArguments
     [(LT typeList GT) (node 'nonWildcardTypeArguments 'LT $2 'GT)]]
    [typeList
     [((sep-by COMMA typeType)) $1]]
    [typeType
     [((* annotation) typeType.2 (* typeType.3)) (node 'typeType $1 $2 $3)]]
    [typeType.2
     [(classOrInterfaceType) (node 'typeType.2 $1)]
     [(primitiveType) (node 'typeType.2 $1)]]
    [typeType.3
     [((* annotation) LBRACK RBRACK) (node 'typeType.3 $1 'LBRACK 'RBRACK)]]
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
     [(typeType GT) (node 'typeArgumentWithClose $1 'GT)]]
    [superSuffix
     [(arguments) (node 'superSuffix $1)]
     [(DOT (? typeArguments) identifier (? arguments)) (node 'superSuffix 'DOT $2 $3 $4)]]
    [explicitGenericInvocationSuffix
     [(SUPER superSuffix) (node 'explicitGenericInvocationSuffix 'SUPER $2)]
     [(identifier arguments) (node 'explicitGenericInvocationSuffix $1 $2)]]
    [arguments
     [(LPAREN (? expressionList) RPAREN) $2]]
    [formalParameter.3
     [((* annotation) ELLIPSIS) (node 'formalParameter.3 $1 'ELLIPSIS)]]
    [enumDeclaration.3
     [(IMPLEMENTS typeList) (node 'enumDeclaration.3 'IMPLEMENTS $2)]]
    [altAnnotationQualifiedName.1
     [(identifier DOT) (node 'altAnnotationQualifiedName.1 $1 'DOT)]]]))

(define (parse-java-code input)
  (define port (open-input-string input))
  (port-count-lines! port)
  (java-parser (lambda () (next-significant-token port))))

(define (parse-java-file path)
  (call-with-input-file path
    (lambda (port)
      (port-count-lines! port)
      (java-parser (lambda () (next-significant-token port))))))
