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

  ;; triple = (list lhs rhs action)
  (define (lower expr defined-lhs stx)
    (match expr
      [(? symbol?)
       (define sugared (maybe-postfix-sugar expr defined-lhs))
       (cond
         [(not sugared)
          (values expr '())]
         [else
          (match sugared
            [(list 'opt inner)
             (define-values (r rs) (lower inner defined-lhs stx))
             (define nt expr)
             (values nt
                     (append rs
                             (list (list nt '() '(quote ()))
                                   (list nt (list r) '$1))))]
            [(list 'rep0 inner)
             (define-values (r rs) (lower inner defined-lhs stx))
             (define nt expr)
             (values nt
                     (append rs
                             (list (list nt '() '(quote ()))
                                   (list nt (list r nt) '(cons $1 $2)))))]
            [(list 'rep1 inner)
             (define-values (r rs) (lower inner defined-lhs stx))
             (define nt+ expr)
             (define nt* (string->symbol
                          (string-append (symbol->string inner) "*")))
             (define user-defined-nt*? (memq nt* defined-lhs))
             (define generated
               (if user-defined-nt*?
                   (list (list nt+ (list r nt*) '(cons $1 $2)))
                   (list (list nt* '() '(quote ()))
                         (list nt* (list r nt*) '(cons $1 $2))
                         (list nt+ (list r nt*) '(cons $1 $2)))))
             (values nt+ (append rs generated))])])]
      [_ (raise-syntax-error 'ext-parser
                             (format "不支持的 EBNF 表达式：~s" expr)
                             stx)]))

  (define (triples->grammar-rules triples)
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
      `(,lhs
        ,@(for/list ([ra (reverse (hash-ref rules-by-lhs lhs))])
            (match-define (list rhs action) ra)
            `(,(if (null? rhs) '() rhs) ,action)))))

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
                           ([item rhs-items] [istx (in-list rhs-stxs)])
                   (define-values (root rs) (lower item defined-lhs istx))
                   (values (cons root roots-rev)
                           (push-list-rev rs extras-rev))))
               (define roots (reverse roots-rev))
               (set! helper-triples-rev (append extras-rev helper-triples-rev))
               (when (null? tail)
                 (raise-syntax-error 'ext-parser
                                     (format "产生式必须提供动作：~s" p)
                                     pstx))
               `(,roots ,@tail)))
           `(,lhs ,@new-prods)]
          [_ (raise-syntax-error 'ext-parser
                                 (format "grammar 规则必须是 [lhs production ...]，实际: ~s" r)
                                 rstx)])))
    `(grammar
      ,@main-rules
      ,@(triples->grammar-rules (reverse helper-triples-rev))))

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

(define-syntax (ext-parser stx)
  (syntax-parse stx
    [(_ clause ...+)
     (define clauses (syntax->list #'(clause ...)))
     (define new-clauses
       (for/list ([c clauses])
         (syntax-local-introduce (transform-clause c))))
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
