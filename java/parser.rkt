#lang racket/base
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         "lexer.rkt"
         "ext-parser.rkt")
(struct ast-compilation-unit (package-declaration import-declarations type-declarations) #:transparent)
(struct ast-modular-compulation-unit (import-declarations module-declaration) #:transparent)
(struct ast-package-declaration (annotations qualified-name) #:transparent)
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
          [(packageDeclaration? compilationUnit.importDeclaration* compilationUnit.typeDeclaration*) (ast-compilation-unit $1 $2 $3)]
          [(modularCompulationUnit) $1]]
         [compilationUnit.importDeclaration
          [(importDeclaration) $1]
          [(SEMI) '()]]
         [compilationUnit.typeDeclaration
          [(typeDeclaration) $1]
          [(SEMI) '()]]
         [modularCompulationUnit
          [(importDeclaration* moduleDeclaration) (ast-modular-compulation-unit $1 $2)]]
         [packageDeclaration
          [(annotation* PACKAGE qualifiedName SEMI) (ast-package-declaration $1 $3)]]
         [importDeclaration
          [(IMPORT importDeclaration.STATIC? importDeclaration.qualifiedName SEMI)]]
         [importDeclaration.STATIC?
          [() #f]
          [(STATIC) #t]]
         [importDeclaration.qualifiedName
          [(qualifiedName) $1]
          [(qualifiedName DOT MUL) (cons $1 "*")]]
         ])))))