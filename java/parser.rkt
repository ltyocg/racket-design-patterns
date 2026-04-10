#lang racket/base
(require parser-tools/lex
         "lexer.rkt"
         "ext-parser.rkt"
         "ast.rkt")

(struct ast-packageDeclaration (annotations name) #:transparent)
(struct ast-importDeclaration (name static asterisk module) #:transparent)
(struct ast-typeDeclaration (annotations name modifiers members) #:transparent)
(struct ast-classOrInterfaceDeclaration (interface compact typeParameters extendedTypes implementedTypes permittedTypes) #:transparent)

(module+ test
  (require racket/pretty)
  (pretty-display
   (syntax->datum
    (expand-once
     #'(ext-parser
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
          [(packageDeclaration? compilationUnit.importDeclaration* compilationUnit.typeDeclaration*) (CompilationUnit $1 $2 $3 null)]
          [(modularCompulationUnit) $1]]
         [compilationUnit.importDeclaration
          [(importDeclaration) $1]
          [(SEMI) '()]]
         [compilationUnit.typeDeclaration
          [(typeDeclaration) $1]
          [(SEMI) '()]]
         [modularCompulationUnit
          [(importDeclaration* moduleDeclaration) (ast-compilationUnit null $1 null $2)]]
         [packageDeclaration
          [(annotation* PACKAGE qualifiedName SEMI) (ast-packageDeclaration $1 $3)]]
         [importDeclaration
          [(IMPORT importDeclaration.static? qualifiedName importDeclaration.asterisk? SEMI) (ast-importDeclaration $3 $2 $4 #f)]]
         [importDeclaration.static?
          [() #f]
          [(STATIC) #t]]
         [importDeclaration.asterisk?
          [() #f]
          [(DOT MUL) #t]]
         [typeDeclaration
          [(classOrInterfaceModifier* typeDeclaration.declaration) (ast-typeDeclaration)]]
         [typeDeclaration.declaration
          [(classDeclaration)]
          [(enumDeclaration)]
          [(interfaceDeclaration)]
          [(annotationTypeDeclaration)]
          [(recordDeclaration)]]
         [modifier
          [(classOrInterfaceModifier) $1]
          [(NATIVE) 'native]
          [(SYNCHRONIZED) 'synchronized]
          [(TRANSIENT) 'transient]
          [(VOLATILE) 'volatile]]
         [classOrInterfaceModifier
          [(annotation) $1]
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
          [(annotation) $1]]
         [classDeclaration
          [(CLASS identifier typeParameters? classDeclaration.extends? classDeclaration.implements? classDeclaration.permits? classBody)
           (ast-classOrInterfaceDeclaration #f #f $3 $4 $5 $6)]]
         ])))))