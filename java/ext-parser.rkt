#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/parse))

(begin-for-syntax
  (define (push-list-rev lst acc-rev)
    (for/fold ([acc acc-rev])
              ([x (in-list lst)])
      (cons x acc)))

  (define current-helper-counter (make-parameter 0))
  (define current-helper-table (make-parameter (make-hash)))
  (define (helper-nt-name key defined-lhs)
    (define tbl (current-helper-table))
    (hash-ref! tbl key
               (lambda ()
                 (let loop ()
                   (define candidate
                     (string->symbol (format "__ext_~a" (current-helper-counter))))
                   (current-helper-counter (add1 (current-helper-counter)))
                   (if (memq candidate defined-lhs)
                       (loop)
                       candidate)))))

  (define (datum->syntax/stx stx v)
    (datum->syntax stx v stx stx))

  ;; --- Grammar operator infrastructure ---

  (struct production-defn (rhs-items action) #:transparent)
  (struct symbol-fn-defn (param productions) #:transparent)

  (struct grammar-operator-info (defn) #:transparent)

  (define grammar-op-prefix "op:")

  (define (grammar-op-id op-stx)
    (datum->syntax op-stx
                   (string->symbol (format "~a~a" grammar-op-prefix (syntax-e op-stx)))))

  ;; --- Grammar transformation ---

  ;; triple = (list lhs rhs action)
  ;; expr-stx is a syntax object; when it's a list starting with an identifier,
  ;; we look it up via syntax-local-value for lexical scoping.
  (define (lower expr-stx defined-lhs)
    (define expr (syntax->datum expr-stx))
    (match expr
      [(list (? symbol? op-sym) _)
       (define op-stx (car (syntax->list expr-stx)))
       (define lookup-stx (grammar-op-id op-stx))
       (define info (syntax-local-value lookup-stx (lambda () #f)))
       (unless (grammar-operator-info? info)
         (raise-syntax-error 'ext-parser
                             (format "未知的文法算子：~a" op-sym)
                             expr-stx))
       (define defn (grammar-operator-info-defn info))
       (define inner-stx (cadr (syntax->list expr-stx)))
       (define-values (r rs) (lower inner-stx defined-lhs))
       (define nt (helper-nt-name (cons op-sym r) defined-lhs))
       (define new-triples
         (for/list ([p (symbol-fn-defn-productions defn)])
           (define resolved-rhs
             (for/list ([item (production-defn-rhs-items p)])
               (cond [(eq? item (symbol-fn-defn-param defn)) r]
                     [(eq? item '_) nt]
                     [else item])))
           (list nt resolved-rhs (production-defn-action p))))
       (values nt (append rs new-triples))]
      [(? symbol?)
       (values expr '())]
      [_ (raise-syntax-error 'ext-parser
                             (format "不支持的 EBNF 表达式：~s" expr)
                             expr-stx)]))

  (define (triples->grammar-rules triples loc-stx)
    (define order-rev '())
    (define rules-by-lhs (make-hash))
    (define seen-by-lhs (make-hash))
    (for ([t triples])
      (match-define (list lhs rhs action) t)
      (unless (hash-has-key? rules-by-lhs lhs)
        (set! order-rev (cons lhs order-rev))
        (hash-set! rules-by-lhs lhs '())
        (hash-set! seen-by-lhs lhs (make-hash)))
      (define candidate (list rhs action))
      (define seen (hash-ref seen-by-lhs lhs))
      (unless (hash-has-key? seen candidate)
        (hash-set! seen candidate #t)
        (hash-update! rules-by-lhs lhs
                      (lambda (old) (cons candidate old)))))
    (for/list ([lhs (reverse order-rev)])
      (datum->syntax/stx loc-stx
                         `(,lhs
                           ,@(for/list ([ra (reverse (hash-ref rules-by-lhs lhs))])
                               (match-define (list rhs action) ra)
                               `(,(if (null? rhs) '() rhs) ,action))))))

  (define (collect-defined-lhs rules-datum)
    (remove-duplicates
     (for/list ([r rules-datum])
       (match r
         [(list (? symbol? lhs) _ ...) lhs]
         [_ (error 'ext-parser
                   (format "grammar 规则必须是 [lhs production ...]，实际: ~s" r))]))))

  (define (normalize-production prod stx)
    (if (and (list? prod) (pair? prod) (list? (car prod)))
        (values (car prod) (cdr prod))
        (raise-syntax-error 'ext-parser
                            (format "production 必须是 [(rhs-item ...) action ...]，实际: ~s" prod)
                            stx)))

  (define (transform-grammar-datum rules-stx rule-stxs)
    (parameterize ([current-helper-counter 0]
                   [current-helper-table (make-hash)])
      (define rules-datum (map syntax->datum rules-stx))
      (define defined-lhs (collect-defined-lhs rules-datum))
      (define helper-triples-rev '())
      (define main-rules
        (for/list ([r rules-datum] [rstx rule-stxs])
          (match r
            [(list lhs prod ...)
             (define prod-stxs (cdr (syntax->list rstx)))
             (define new-prods
               (for/list ([p prod] [pstx prod-stxs])
                 (define-values (rhs-items tail) (normalize-production p pstx))
                 (define rhs-stxs (syntax->list (car (syntax->list pstx))))
                 (define-values (roots-rev extras-rev)
                   (for/fold ([roots-rev '()]
                              [extras-rev '()])
                             ([_ rhs-items] [istx (in-list rhs-stxs)])
                     (define-values (root rs) (lower istx defined-lhs))
                     (values (cons root roots-rev)
                             (push-list-rev rs extras-rev))))
                 (define roots (reverse roots-rev))
                 (set! helper-triples-rev (append extras-rev helper-triples-rev))
                 (when (null? tail)
                   (raise-syntax-error 'ext-parser
                                       (format "产生式必须提供动作：~s" p)
                                       pstx))
                 (datum->syntax/stx pstx `(,roots ,@tail))))
             (datum->syntax/stx rstx `(,lhs ,@new-prods))]
            [_ (raise-syntax-error 'ext-parser
                                   (format "grammar 规则必须是 [lhs production ...]，实际: ~s" r)
                                   rstx)])))
      (datum->syntax/stx (car rules-stx)
                         `(grammar
                           ,@main-rules
                           ,@(triples->grammar-rules (reverse helper-triples-rev) (car rules-stx))))))

  (define (transform-clause c)
    (define parts (syntax->list c))
    (if (and parts
             (pair? parts)
             (eq? (syntax-e (car parts)) 'grammar))
        (datum->syntax c
                       (transform-grammar-datum
                        (cdr parts)
                        (cdr parts))
                       c c)
        c)))

;; --- User-facing macro ---

(define-syntax (define-grammar-operator stx)
  (syntax-parse stx
    [(_ (op:id param:id) [(~datum _) [rhs action] ...])
     (define param-sym (syntax-e #'param))
     (define rhs-stxs (syntax->list #'(rhs ...)))
     (define act-stxs (syntax->list #'(action ...)))
     (define prods
       (for/list ([rhs-stx rhs-stxs] [act-stx act-stxs])
         (list (map syntax-e (syntax->list rhs-stx))
               (syntax->datum act-stx))))
     #`(define-syntax #,(grammar-op-id #'op)
         (grammar-operator-info
          (symbol-fn-defn '#,param-sym
                          (list #,@(for/list ([p prods])
                                     #`(production-defn '#,(car p) '#,(cadr p)))))))]))

(define-syntax (ext-parser stx)
  (syntax-parse stx
    [(_ clause ...+)
     (define clauses (syntax->list #'(clause ...)))
     (define new-clauses
       (for/list ([c clauses])
         (syntax-local-introduce (transform-clause c))))
     #`(parser #,@new-clauses)]))

(provide ext-parser define-grammar-operator)

(module+ test
  (require racket/pretty)
  (define-tokens value-tokens (NUM))
  (define-empty-tokens op-tokens (PLUS LPAREN RPAREN EOF))
  (define-grammar-operator (? s)
    [_
     [() '()]
     [(s) $1]])
  (define-grammar-operator (* s)
    [_
     [() '()]
     [(s _) (cons $1 $2)]])
  (define-grammar-operator (+ s)
    [_
     [(s) (cons $1 '())]
     [(s _) (cons $1 $2)]])
  (pretty-display (+ 1 2))
  (pretty-display
   (syntax->datum
    (expand-once
     #'(ext-parser
        [start expr]
        [end EOF]
        [error (lambda args (error 'my-parser (format "parse error: ~s" args)))]
        [src-pos]
        [tokens value-tokens op-tokens]
        [grammar
         [expr
          [(term) $1]
          [((? NUM)) $1]]
         [term
          [((? NUM) (* LPAREN) (+ RPAREN)) (list $1 $2 $3)]]]))))
  (pretty-display
   (syntax->datum
    (expand-once
     #'(ext-parser
        [start expr]
        [end EOF]
        [error (lambda args (error 'my-parser (format "parse error: ~s" args)))]
        [tokens value-tokens op-tokens]
        [grammar
         [expr
          [((+ NUM)) $1]]]))))
  (pretty-display
   (syntax->datum
    (expand-once
     #'(ext-parser
        [start expr]
        [end EOF]
        [error (lambda args (error 'my-parser (format "parse error: ~s" args)))]
        [src-pos]
        [tokens value-tokens op-tokens]
        [grammar
         [expr
          [((? (* NUM))) $1]]])))))
