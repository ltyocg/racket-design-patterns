#lang racket/base
(require parser-tools/lex
         "lexer.rkt"
         "ext-parser.rkt"
         "ast.rkt")
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
  ;  [yacc-output "parser.o"]
  ;  [debug "parser-debug.txt"]
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
     [(IMPORT (?->bool STATIC) qualifiedName importDeclaration.4 SEMI) todo]]
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
     [(LT (sep-by COMMA typeParameter) GT) $2]]
    [typeParameter
     [((* annotation) identifier (? typeParameter.3)) todo]]
    [typeParameter.3
     [(EXTENDS (* annotation) typeBound) todo]]
    [typeBound
     [((sep-by BITAND typeType)) $1]]
    [enumDeclaration
     [(ENUM identifier (? enumDeclaration.3) LBRACE (? enumConstants) (?->bool COMMA) (? enumBodyDeclarations) RBRACE) todo]]
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
     [((?->bool STATIC) block) todo]
     [((* modifier) memberDeclaration) todo]]
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
     [((* annotation) typeTypeOrVoid identifier formalParameters (* brackets) (? interfaceCommonBodyDeclaration.6) methodBody) todo]]
    [interfaceCommonBodyDeclaration.6
     [(THROWS qualifiedNameList) todo]]
    [variableDeclarators
     [((sep-by COMMA variableDeclarator)) $1]]
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
     [((sep-by COMMA variableInitializer) (?->bool COMMA)) todo]]
    [classType
     [((+ classType.1) (* classType.2)) todo]]
    [classType.1
     [((? classType.1.1) typeIdentifier (? typeArguments)) todo]]
    [classType.1.1
     [(packageName DOT (* annotation)) todo]]
    [classType.2
     [(DOT (* annotation) typeIdentifier (? typeArguments)) todo]]
    [packageName
     [((sep-by DOT identifier)) $1]]
    [typeArgument
     [(typeType) todo]
     [((* annotation) QUESTION (? typeArgument.3)) todo]]
    [typeArgument.3
     [(typeArgument.3.1 typeType) todo]]
    [typeArgument.3.1
     [(EXTENDS) 'extends]
     [(SUPER) 'super]]
    [qualifiedNameList
     [((sep-by COMMA qualifiedName)) $1]]
    [formalParameters
     [(LPAREN (? formalParameters.2) RPAREN) $2]]
    [formalParameters.2
     [(formalParameters.2.1 (* formalParameters.2.2)) todo]]
    [formalParameters.2.1
     [(receiverParameter) todo]
     [(formalParameter) todo]]
    [formalParameters.2.2
     [(COMMA formalParameterList) $2]]
    [receiverParameter
     [(typeType (* receiverParameter.2) THIS) todo]]
    [receiverParameter.2
     [(identifier DOT) todo]]
    [formalParameterList
     [((sep-by COMMA formalParameter)) $1]]
    [formalParameter
     [((* variableModifier) typeType (? formalParameter.3) variableDeclaratorId) todo]]
    [lambdaLVTIList
     [((sep-by COMMA lambdaLVTIParameter)) $1]]
    [lambdaLVTIParameter
     [((* variableModifier) VAR identifier) todo]]
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
     [((* altAnnotationQualifiedName.1) AT identifier) todo]]
    [annotation
     [(annotation.1 (? annotationFieldValues)) todo]]
    [annotation.1
     [(AT qualifiedName) $2]]
    [annotationFieldValues
     [(LPAREN (? (sep-by COMMA annotationFieldValue)) RPAREN) $2]]
    [annotationFieldValue
     [(identifier ASSIGN annotationValue) todo]
     [(annotationValue) todo]]
    [annotationValue
     [(expression) todo]
     [(annotation) todo]
     [(LBRACE (? (sep-by COMMA annotationValue)) (?->bool COMMA) RBRACE) $2]]
    [elementValue
     [(expression) todo]
     [(annotation) todo]
     [(elementValueArrayInitializer) todo]]
    [elementValueArrayInitializer
     [(LBRACE (? (sep-by COMMA elementValue)) (?->bool COMMA) RBRACE) todo]]
    [annotationTypeDeclaration
     [(AT INTERFACE identifier annotationTypeBody) todo]]
    [annotationTypeBody
     [(LBRACE (* annotationTypeElementDeclaration) RBRACE) todo]]
    [annotationTypeElementDeclaration
     [((* modifier) annotationTypeElementRest) todo]
     [(SEMI) null]]
    [annotationTypeElementRest
     [(typeType annotationMethodOrConstantRest SEMI) todo]
     [(classDeclaration (?->bool SEMI)) $1]
     [(interfaceDeclaration (?->bool SEMI)) $1]
     [(enumDeclaration (?->bool SEMI)) $1]
     [(annotationTypeDeclaration (?->bool SEMI)) $1]
     [(recordDeclaration (?->bool SEMI)) $1]]
    [annotationMethodOrConstantRest
     [(annotationMethodRest) $1]
     [(annotationConstantRest) $1]]
    [annotationMethodRest
     [(identifier LPAREN RPAREN (? defaultValue)) todo]]
    [annotationConstantRest
     [(variableDeclarators) todo]]
    [defaultValue
      [(DEFAULT elementValue) todo]]
    [moduleDeclaration
     [((* annotation) (?->bool OPEN) MODULE qualifiedName LBRACE (* moduleDirective) RBRACE) todo]]
    [moduleDirective
     [(REQUIRES (* requiresModifier) qualifiedName SEMI) todo]
     [(EXPORTS qualifiedName (? moduleDirective.3) SEMI) todo]
     [(OPENS qualifiedName (? moduleDirective.3) SEMI) todo]
     [(USES qualifiedName SEMI) todo]
     [(PROVIDES qualifiedName WITH (sep-by COMMA qualifiedName) SEMI) todo]]
    [moduleDirective.3
     [(TO (sep-by COMMA qualifiedName)) todo]]
    [requiresModifier
     [(TRANSITIVE) 'transitive]
     [(STATIC) 'static]]
    [recordDeclaration
     [(RECORD identifier (? typeParameters) recordHeader (? recordDeclaration.5) recordBody) todo]]
    [recordDeclaration.5
     [(IMPLEMENTS typeList) $2]]
    [recordHeader
     [(LPAREN (? recordComponentList) RPAREN) $2]]
    [recordComponentList
     [((sep-by COMMA recordComponent)) $1]]
    [recordComponent
     [((* annotation) typeType (? recordComponent.3) identifier) todo]]
    [recordComponent.3
     [((* annotation) ELLIPSIS) todo]]
    [recordBody
     [(LBRACE (* recordBody.2) RBRACE) todo]]
    [recordBody.2
     [(classBodyDeclaration) $1]
     [(compactConstructorDeclaration) $1]]
    [block
     [(LBRACE (* blockStatement) RBRACE) $2]]
    [blockStatement
     [(localVariableDeclaration SEMI) $1]
     [(localTypeDeclaration) $1]
     [(statement) $1]]
    [localVariableDeclaration
     [((* variableModifier) localVariableDeclaration.2) todo]]
    [localVariableDeclaration.2
     [(VAR identifier ASSIGN expression) todo]
     [(typeType variableDeclarators) todo]]
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
     [((* classOrInterfaceModifier) localTypeDeclaration.2) todo]]
    [localTypeDeclaration.2
     [(classDeclaration) $1]
     [(interfaceDeclaration) $1]
     [(recordDeclaration) $1]
     [(enumDeclaration) $1]]
    [statement
     [(block) $1] ;blockLabel = block
     [(ASSERT expression (? statement.3/2) SEMI) todo]
     [(IF LPAREN expression RPAREN statement (? statement.6)) todo]
     [(FOR LPAREN forControl RPAREN statement) todo]
     [(WHILE LPAREN expression RPAREN statement) todo]
     [(DO statement WHILE LPAREN expression RPAREN SEMI) todo]
     [(TRY block statement.3/7) todo]
     [(TRY resourceSpecification block (* catchClause) (? finallyBlock)) todo]
     [(SWITCH LPAREN expression RPAREN LBRACE (* switchBlockStatementGroup) (* switchLabel) RBRACE) todo]
     [(SYNCHRONIZED RPAREN expression RPAREN block) todo]
     [(RETURN (? expression) SEMI) todo]
     [(THROW expression SEMI) todo]
     [(BREAK (? identifier) SEMI) todo]
     [(CONTINUE (? identifier) SEMI) todo]
     [(YIELD (? expression) SEMI) todo]
     [(SEMI) null]
     [(expression SEMI) todo] ;statementExpression = expression
     [(switchExpression (?->bool SEMI)) todo]
     [(identifier COLON statement) todo]] ;identifierLabel = identifier
    [statement.3/2
     [(COLON expression) $2]]
    [statement.6
     [(ELSE statement) $2]]
    [statement.3/7
     [((+ catchClause) (? finallyBlock)) todo]
     [(finallyBlock) todo]]
    [catchClause
     [(CATCH LPAREN (* variableModifier) catchType identifier RPAREN block) todo]]
    [catchType
     [((sep-by BITOR qualifiedName)) $1]]
    [finallyBlock
     [(FINALLY block) todo]]
    [resourceSpecification
     [(LPAREN resources (?->bool SEMI) RPAREN) todo]]
    [resources
     [((sep-by SEMI resource)) $1]]
    [resource
     [((* variableModifier) classOrInterfaceType variableDeclaratorId ASSIGN expression) todo]
     [((* variableModifier) VAR identifier ASSIGN expression) todo]
     [(qualifiedName) todo]]
    [switchBlockStatementGroup
     [((+ switchBlockStatementGroup.1) (+ blockStatement)) todo]]
    [switchBlockStatementGroup.1
     [(switchLabel COLON) todo]]
    [switchLabel
     [(CASE expression) todo]
     [(CASE IDENTIFIER) todo]
     [(CASE typeType identifier) todo]
     [(DEFAULT) 'default]]
    [forControl
     [(enhancedForControl) todo]
     [((? forInit) SEMI (? expression) SEMI (? expressionList)) todo]]
    [forInit
     [(localVariableDeclaration) todo]
     [(expressionList) todo]]
    [enhancedForControl
     [((* variableModifier) typeType variableDeclaratorId COLON expression) todo]
     [((* variableModifier) VAR variableDeclaratorId COLON expression) todo]]
    [expressionList
     [((sep-by COMMA expression)) $1]]
    [methodCall
     [(identifier arguments) todo]
     [(THIS arguments) todo]
     [(SUPER arguments) todo]]
    [expression
     [(primary) todo]
     [(expression LBRACK expression RBRACK) todo]
     [(expression DOT identifier) todo]
     [(expression DOT methodCall) todo]
     [(expression DOT THIS) todo]
     [(expression DOT NEW (? nonWildcardTypeArguments) innerCreator) todo]
     [(expression DOT SUPER superSuffix) todo]
     [(expression DOT explicitGenericInvocation) todo]
     [(methodCall) todo]
     [(expression COLONCOLON (? typeArguments) identifier) todo]
     [(typeType COLONCOLON (? typeArguments) identifier) todo]
     [(typeType COLONCOLON NEW) todo]
     [(classType COLONCOLON (? typeArguments) NEW) todo]
     [(switchExpression) todo]
     [(expression expression.18) todo] ;postfix
     [(expression.20 expression) todo] ;prefix
     [(LPAREN (* annotation) typeType (* expression.23) RPAREN expression) todo] ;cast
     [(NEW creator) todo]
     [(expression expression.26 expression) todo] ;bop */%
     [(expression expression.27 expression) todo] ;bop +-
     [(expression expression.28 expression) todo] ;bop << >> >>>
     [(expression expression.29 expression) todo] ;bop <= >= > <
     [(expression INSTANCEOF typeType) todo]
     [(expression INSTANCEOF pattern) todo]
     [(expression expression.32 expression) todo] ;bop == !=
     [(expression BITAND expression) todo]
     [(expression CARET expression) todo]
     [(expression BITOR expression) todo]
     [(expression AND expression) todo]
     [(expression OR expression) todo]
     [(expression QUESTION expression COLON expression) todo]
     [(expression expression.39 expression) todo] ;bop assignment
     [(lambdaExpression) todo]]
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
     [(BITAND typeType) todo]]
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
      [((* variableModifier) typeType (* annotation) variableDeclarators) todo]
      [(typeType LPAREN (? componentPatternList) RPAREN) todo]]
    [componentPatternList
     [((sep-by COMMA componentPattern)) $1]]
    [componentPattern
     [(pattern) todo]]
    [lambdaExpression
     [(lambdaParameters ARROW lambdaBody) todo]]
    [lambdaParameters
     [(identifier) todo]
     [(LPAREN (? formalParameterList) RPAREN) todo]
     [(LPAREN (sep-by COMMA identifier) RPAREN) todo]
     [(LPAREN (? lambdaLVTIList) RPAREN) todo]]
    [lambdaBody
     [(expression) todo]
     [(block) todo]]
    [primary
     [(LPAREN expression RPAREN) todo]
     [(THIS) 'this]
     [(SUPER) 'super]
     [(literal) todo]
     [(identifier) todo]
     [(typeTypeOrVoid DOT CLASS) todo]
     [(nonWildcardTypeArguments primary.7) todo]]
    [primary.7
     [(explicitGenericInvocationSuffix) todo]
     [(THIS arguments) todo]]
    [switchExpression
     [(SWITCH LPAREN expression RPAREN LBRACE (* switchLabeledRule) RBRACE) todo]]
    [switchLabeledRule
     [(CASE expressionList switchLabeledRule.3 switchRuleOutcome) todo]
     [(CASE NULL_LITERAL switchLabeledRule.2.2 switchLabeledRule.3 switchRuleOutcome) todo]
     [(CASE (+ casePattern) (? guard) switchLabeledRule.3 switchRuleOutcome) todo]
     [(DEFAULT switchLabeledRule.3 switchRuleOutcome) todo]]
    [switchLabeledRule.2.2
     [() #f]
     [(COMMA DEFAULT) #t]]
    [switchLabeledRule.3
     [(ARROW) 'arrow]
     [(COLON) 'colon]]
    [guard
     [(WHEN expression) todo]]
    [casePattern
     [(pattern) todo]]
    [switchRuleOutcome
     [(block) todo]
     [((* blockStatement)) todo]]
    [classOrInterfaceType
     [(classType) todo]]
    [creator
     [((? nonWildcardTypeArguments) createdName classCreatorRest) todo]
     [(createdName arrayCreatorRest) todo]]
    [createdName
     [(identifier (? typeArgumentsOrDiamond) (* createdName.3)) todo]
     [(primitiveType) todo]]
    [createdName.3
     [(DOT identifier (? typeArgumentsOrDiamond)) todo]]
    [innerCreator
     [(identifier (? nonWildcardTypeArgumentsOrDiamond) classCreatorRest) todo]]
    [arrayCreatorRest
     [((+ brackets) arrayInitializer) todo]
     [((+ arrayCreatorRest.2) (* brackets)) todo]]
    [arrayCreatorRest.2
     [(LBRACK expression RBRACK) todo]]
    [classCreatorRest
     [(arguments (? classBody)) todo]]
    [explicitGenericInvocation
     [(nonWildcardTypeArguments explicitGenericInvocationSuffix) todo]]
    [typeArgumentsOrDiamond
     [(LT GT) todo]
     [(typeArguments) todo]]
    [nonWildcardTypeArgumentsOrDiamond
     [(LT GT) todo]
     [(nonWildcardTypeArguments) todo]]
    [nonWildcardTypeArguments
     [(LT typeList GT) todo]]
    [typeList
     [((sep-by COMMA typeType)) $1]]
    [typeType
     [((* annotation) typeType.2 (* typeType.3)) todo]]
    [typeType.2
     [(classOrInterfaceType) todo]
     [(primitiveType) todo]]
    [typeType.3
     [((* annotation) LBRACK RBRACK) todo]]
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
     [(LT (sep-by COMMA typeArgument) GT) $2]]
    [superSuffix
     [(arguments) todo]
     [(DOT (? typeArguments) identifier (? arguments)) todo]]
    [explicitGenericInvocationSuffix
     [(SUPER superSuffix) todo]
     [(identifier arguments) todo]]
    [arguments
     [(LPAREN (? expressionList) RPAREN) $2]]
    [formalParameter.3
     [((* annotation) ELLIPSIS) todo]]
    [enumDeclaration.3
     [(IMPLEMENTS typeList) todo]]
    [altAnnotationQualifiedName.1
     [(identifier DOT) todo]]]))
(define (parse-java-code input)
  (define port (open-input-string input))
  (java-parser (lambda () (java-lexer port))))
(module+ main
  (require racket/pretty)
  (define (expand-once/print stx)
    (let ([expanded (expand-once stx)])
      (write (syntax->datum expanded))
      (newline)
      expanded))
  (expand-once/print
   #'(+)))