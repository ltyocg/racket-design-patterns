#lang racket/base

(require racket/list
         racket/match)

(provide (struct-out ast-node)
         (struct-out ast-token)
         ast?
         ast-kind
         ast-children
         ast-token-hidden?
         ast-token-visible?
         ast-tokens
         ast-token-texts
         ast-filter
         ast-filter-kind
         ast-find
         ast-find-kind
         parse-tree->ast
         ast->datum)

;; A small, grammar-agnostic AST facade over the ANTLR parse tree.
;;
;; Rule nodes keep the ANTLR rule name in `kind`; token nodes keep the ANTLR
;; token name, source text, source span, and token channel. This deliberately
;; avoids the old parser-tools-era model where every Java grammar rule had its
;; own struct.
(struct ast-node (kind children) #:transparent)
(struct ast-token (kind text line column start stop channel) #:transparent)

(define (ast? value)
  (or (ast-node? value) (ast-token? value)))

(define (ast-kind value)
  (cond
    [(ast-node? value) (ast-node-kind value)]
    [(ast-token? value) (ast-token-kind value)]
    [else (raise-argument-error 'ast-kind "ast?" value)]))

(define (ast-children value)
  (cond
    [(ast-node? value) (ast-node-children value)]
    [(ast-token? value) '()]
    [else (raise-argument-error 'ast-children "ast?" value)]))

(define (parse-tree->ast datum)
  (cond
    [(ast? datum) datum]
    [else
     (match datum
       [(list 'token (? symbol? kind) (? string? text)
              (? exact-integer? line) (? exact-integer? column)
              (? exact-integer? start) (? exact-integer? stop)
              (? symbol? channel))
        (ast-token kind text line column start stop channel)]
       [(list 'token (? symbol? kind) (? string? text)
              (? exact-integer? line) (? exact-integer? column)
              (? exact-integer? start) (? exact-integer? stop))
        (ast-token kind text line column start stop 'DEFAULT)]
       [(cons (? symbol? kind) children)
        (ast-node kind (map parse-tree->ast children))]
       [_
        (raise-argument-error 'parse-tree->ast
                              "ANTLR parse-tree datum"
                              datum)])]))

(define (ast->datum value #:include-channel? [include-channel? #t])
  (cond
    [(ast-node? value)
     (cons (ast-node-kind value)
           (map (lambda (child) (ast->datum child #:include-channel? include-channel?))
                (ast-node-children value)))]
    [(ast-token? value)
     (define datum
       (list 'token
             (ast-token-kind value)
             (ast-token-text value)
             (ast-token-line value)
             (ast-token-column value)
             (ast-token-start value)
             (ast-token-stop value)))
     (if include-channel?
         (append datum (list (ast-token-channel value)))
         datum)]
    [else (raise-argument-error 'ast->datum "ast?" value)]))

(define (ast-token-hidden? value)
  (and (ast-token? value)
       (not (eq? (ast-token-channel value) 'DEFAULT))))

(define (ast-token-visible? value)
  (and (ast-token? value)
       (not (ast-token-hidden? value))))

(define (keep-token? token include-eof? include-hidden?)
  (and (or include-eof? (not (eq? (ast-token-kind token) 'EOF)))
       (or include-hidden? (not (ast-token-hidden? token)))))

(define (ast-tokens value
                    #:include-eof? [include-eof? #f]
                    #:include-hidden? [include-hidden? #t])
  (cond
    [(ast-token? value)
     (if (keep-token? value include-eof? include-hidden?)
         (list value)
         '())]
    [(ast-node? value)
     (append-map (lambda (child)
                   (ast-tokens child
                               #:include-eof? include-eof?
                               #:include-hidden? include-hidden?))
                 (ast-node-children value))]
    [else (raise-argument-error 'ast-tokens "ast?" value)]))

(define (ast-token-texts value
                         #:include-eof? [include-eof? #f]
                         #:include-hidden? [include-hidden? #t])
  (map ast-token-text
       (ast-tokens value
                   #:include-eof? include-eof?
                   #:include-hidden? include-hidden?)))

(define (ast-filter pred value)
  (unless (procedure? pred)
    (raise-argument-error 'ast-filter "procedure?" pred))
  (cond
    [(ast-node? value)
     (append (if (pred value) (list value) '())
             (append-map (lambda (child) (ast-filter pred child))
                         (ast-node-children value)))]
    [(ast-token? value)
     (if (pred value) (list value) '())]
    [else (raise-argument-error 'ast-filter "ast?" value)]))

(define (ast-filter-kind kind value)
  (unless (symbol? kind)
    (raise-argument-error 'ast-filter-kind "symbol?" kind))
  (ast-filter (lambda (child) (eq? (ast-kind child) kind)) value))

(define (ast-find pred value)
  (unless (procedure? pred)
    (raise-argument-error 'ast-find "procedure?" pred))
  (cond
    [(not (ast? value))
     (raise-argument-error 'ast-find "ast?" value)]
    [(pred value) value]
    [(ast-node? value)
     (let loop ([children (ast-node-children value)])
       (cond
         [(null? children) #f]
         [else
          (or (ast-find pred (car children))
              (loop (cdr children)))]))]
    [else #f]))

(define (ast-find-kind kind value)
  (unless (symbol? kind)
    (raise-argument-error 'ast-find-kind "symbol?" kind))
  (ast-find (lambda (child) (eq? (ast-kind child) kind)) value))

(module+ test
  (require rackunit)

  (define datum
    '(compilationunit
      (typeDeclaration
       (classDeclaration
        (token CLASS "class" 1 0 0 4 DEFAULT)
        (identifier (token IDENTIFIER "C" 1 6 6 6 DEFAULT))))
      (token EOF "<EOF>" 1 7 7 6 DEFAULT)))

  (define ast (parse-tree->ast datum))

  (check-true (ast-node? ast))
  (check-equal? (ast-node-kind ast) 'compilationunit)
  (check-equal? (ast-kind (ast-find-kind 'CLASS ast)) 'CLASS)
  (check-equal? (ast-token-channel (ast-find-kind 'CLASS ast)) 'DEFAULT)
  (check-equal? (ast-token-texts ast) '("class" "C"))
  (check-equal? (ast->datum ast) datum)

  (define old-token-datum '(token IDENTIFIER "C" 1 0 0 0))
  (define old-token (parse-tree->ast old-token-datum))
  (check-equal? (ast-token-channel old-token) 'DEFAULT)
  (check-equal? (ast->datum old-token #:include-channel? #f) old-token-datum))
