#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/parse
                     parser-tools/private-lex/token-syntax))

(begin-for-syntax
  (define (d$ n)
    (string->symbol (format "$~a" n)))

  (define (maybe-postfix-sugar sym defined-lhs)
    (if (memq sym defined-lhs)
        #f
        (let* ([str (symbol->string sym)]
               [n (string-length str)]
               [last (and (> n 1) (string-ref str (sub1 n)))])
          (cond
            [(eqv? last #\?)
             `(opt ,(string->symbol (substring str 0 (sub1 n))))]
            [(eqv? last #\*)
             `(rep0 ,(string->symbol (substring str 0 (sub1 n))))]
            [(eqv? last #\+)
             `(rep1 ,(string->symbol (substring str 0 (sub1 n))))]
            [else #f]))))

  ;; triple = (list lhs rhs tag)
  (define (lower expr defined-lhs)
    (match expr
      [(? symbol?)
       (define sugared (maybe-postfix-sugar expr defined-lhs))
       (cond
         [(not sugared)
          (values expr '())]
         [else
          (match sugared
            [(list 'opt inner)
             (define-values (r rs) (lower inner defined-lhs))
             (define nt expr)
             (values nt
                     (append rs
                             (list (list nt '() 'opt-empty)
                                   (list nt (list r) 'opt-some))))]
            [(list 'rep0 inner)
             (define-values (r rs) (lower inner defined-lhs))
             (define nt expr)
             (values nt
                     (append rs
                             (list (list nt '() 'rep0-empty)
                                   (list nt (list r nt) 'rep0-step))))]
            [(list 'rep1 inner)
             (define-values (r rs) (lower inner defined-lhs))
             (define nt+ expr)
             (define nt* (string->symbol
                          (string-append (symbol->string inner) "*")))
             (define user-defined-nt*? (memq nt* defined-lhs))
             (define generated
               (if user-defined-nt*?
                   (list (list nt+ (list r nt*) 'rep1-step))
                   (list (list nt* '() 'rep0-empty)
                         (list nt* (list r nt*) 'rep0-step)
                         (list nt+ (list r nt*) 'rep1-step))))
             (values nt+ (append rs generated))])])]

      [_ (error 'ext-parser (format "不支持的 EBNF 表达式：~s" expr))]))

  (define (collect-empty-terminals clauses)
    (define token-groups
      (for*/list ([c clauses]
                  [ids (in-value
                        (let ([parts (syntax->list c)])
                          (if (and parts
                                   (pair? parts)
                                   (eq? (syntax-e (car parts)) 'tokens))
                              (cdr parts)
                              '())))]
                  [id ids])
        id))
    (define empty-token-syms '())
    (for ([tg token-groups])
      (define v (syntax-local-value tg (lambda () #f)))
      (when (e-terminals-def? v)
        (set! empty-token-syms
              (append empty-token-syms
                      (map syntax-e (syntax->list (e-terminals-def-t v)))))))
    empty-token-syms)

  (define (accessible-indexes rhs empty-token-syms)
    (for/list ([sym rhs] [i (in-naturals 1)]
                         #:unless (memq sym empty-token-syms))
      i))

  (define (pick-action-from-indexes idxs)
    (cond
      [(null? idxs) '(quote ())]
      [(null? (cdr idxs)) (d$ (car idxs))]
      [else `(list ,@(map d$ idxs))]))

  (define (default-action rhs empty-token-syms)
    (pick-action-from-indexes (accessible-indexes rhs empty-token-syms)))

  (define (action-for tag rhs empty-token-syms)
    (define idxs (accessible-indexes rhs empty-token-syms))
    (match tag
      ['opt-empty '(quote ())]
      ['opt-some (if (null? idxs) #f (d$ (car idxs)))]
      ['rep0-empty '(quote ())]
      ['rep0-step
       (cond
         [(and (member 1 idxs) (member 2 idxs)) '(cons $1 $2)]
         [(member 1 idxs) '(list $1)]
         [(member 2 idxs) '$2]
         [else '(quote ())])]
      ['rep1-step
       (cond
         [(and (member 1 idxs) (member 2 idxs)) '(cons $1 $2)]
         [(member 1 idxs) '(list $1)]
         [(member 2 idxs) '$2]
         [else '(quote ())])]
      ['eps '(quote ())]
      [_ (pick-action-from-indexes idxs)]))

  (define (triples->grammar-rules triples empty-token-syms)
    (define order '())
    (define tbl (make-hash)) ; lhs -> (listof (list rhs action))
    (for ([t triples])
      (match-define (list lhs rhs tag) t)
      (define action (action-for tag rhs empty-token-syms))
      (unless (hash-has-key? tbl lhs)
        (set! order (append order (list lhs))))
      (hash-update! tbl lhs
                    (lambda (old)
                      (define candidate (list rhs action))
                      (if (member candidate old equal?)
                          old
                          (append old (list candidate))))
                    '()))
    (for/list ([lhs order])
      `(,lhs
        ,@(for/list ([ra (hash-ref tbl lhs)])
            (match-define (list rhs action) ra)
            `(,(if (null? rhs) '() rhs) ,action)))))

  (define (collect-defined-lhs rules-datum)
    (remove-duplicates
     (for/list ([r rules-datum])
       (match r
         [(list (? symbol? lhs) _ ...) lhs]
         [_ (error 'ext-parser
                   (format "grammar 规则必须是 [lhs production ...]，实际: ~s" r))]))))

  (define (normalize-production prod)
    (if (and (list? prod) (pair? prod) (list? (car prod)))
        (values (car prod) (cdr prod))
        (error 'ext-parser
               (format "production 必须是 [(rhs-item ...) action ...]，实际: ~s" prod))))

  (define (transform-grammar-datum rules-datum empty-token-syms)
    (define defined-lhs (collect-defined-lhs rules-datum))
    (define helper-triples '())
    (define main-rules
      (for/list ([r rules-datum])
        (match r
          [(list lhs prod ...)
           (define new-prods
             (for/list ([p prod])
               (define-values (rhs-items tail) (normalize-production p))
               (define roots '())
               (define extras '())
               (for ([item rhs-items])
                 (define-values (root rs) (lower item defined-lhs))
                 (set! roots (append roots (list root)))
                 (set! extras (append extras rs)))
               (set! helper-triples (append helper-triples extras))
               (define final-tail
                 (if (null? tail)
                     (list (default-action roots empty-token-syms))
                     tail))
               `(,roots ,@final-tail)))
           `(,lhs ,@new-prods)]
          [_ (error 'ext-parser
                    (format "grammar 规则必须是 [lhs production ...]，实际: ~s" r))])))
    `(grammar
      ,@main-rules
      ,@(triples->grammar-rules helper-triples empty-token-syms)))

  (define (transform-clause c empty-token-syms)
    (define parts (syntax->list c))
    (if (and parts
             (pair? parts)
             (eq? (syntax-e (car parts)) 'grammar))
        (datum->syntax c
                       (transform-grammar-datum
                        (map syntax->datum (cdr parts))
                        empty-token-syms)
                       c c)
        c)))

(define-syntax (ext-parser stx)
  (syntax-parse stx
    [(_ clause ...+)
     (define clauses (syntax->list #'(clause ...)))
     (define empty-token-syms (collect-empty-terminals clauses))
     (define new-clauses
       (for/list ([c clauses])
         (syntax-local-introduce (transform-clause c empty-token-syms))))
     #`(parser #,@new-clauses)]))

(provide ext-parser)

(module+ test
  (require racket/pretty)
  (define-tokens value-tokens (NUM))
  (define-empty-tokens op-tokens (PLUS LPAREN RPAREN EOF))
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
          [(NUM?) $1]]
         [term
          [(NUM? LPAREN* RPAREN+) (list $1 $2 $3)]]]))))
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
          [(NUM+) $1]]
         [NUM*
          [() 'user-empty]
          [(NUM NUM*) (cons $1 $2)]]])))))
