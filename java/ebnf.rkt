#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/string
                     syntax/parse
                     parser-tools/private-lex/token-syntax))

(begin-for-syntax
  (define gensym-counter 0)
  (define (reset-gensym!)
    (set! gensym-counter 0))
  (define (fresh base)
    (set! gensym-counter (add1 gensym-counter))
    (string->symbol
     (format "__ext_~a_~a"
             (regexp-replace* #rx"[^0-9A-Za-z_]+" (symbol->string base) "_")
             gensym-counter)))
  (define (d$ n)
    (string->symbol (format "$~a" n)))

  (define (symbol-prefix? s prefix)
    (string-prefix? (symbol->string s) prefix))

  (define (ensure-plain-symbol sym where)
    (when (symbol-prefix? sym "~")
      (error 'ext-parser
             (format "~a 中的符号不能以 `~` 开头：~s" where sym))))

  (define (maybe-postfix-sugar sym defined-lhs)
    (if (memq sym defined-lhs)
        #f
        (let* ([str (symbol->string sym)]
               [n (string-length str)]
               [last (and (> n 1) (string-ref str (sub1 n)))])
          (cond
            [(eqv? last #\?)
             `(~opt ,(string->symbol (substring str 0 (sub1 n))))]
            [(eqv? last #\*)
             `(~rep0 ,(string->symbol (substring str 0 (sub1 n))))]
            [(eqv? last #\+)
             `(~rep1 ,(string->symbol (substring str 0 (sub1 n))))]
            [else #f]))))

  (define (operator-kind head defined-lhs)
    (and (symbol? head)
         (not (memq head defined-lhs))
         (cond
           [(memq head '(~seq seq)) 'seq]
           [(memq head '(~or or alt)) 'or]
           [(memq head '(~opt opt ?)) 'opt]
           [(memq head '(~rep0 rep rep0 *)) 'rep0]
           [(memq head '(~rep1 rep1 +)) 'rep1]
           [else #f])))

  ;; triple = (list lhs rhs tag)
  (define (lower expr base defined-lhs)
    (define (lower-seq es)
      (define roots '())
      (define rules '())
      (for ([e es])
        (define-values (r rs) (lower e base defined-lhs))
        (set! roots (append roots (list r)))
        (set! rules (append rules rs)))
      (define nt (fresh base))
      (values nt
              (append rules
                      (list (list nt roots (if (null? roots) 'eps 'seq))))))

    (match expr
      [(? symbol?)
       (ensure-plain-symbol expr "EBNF 表达式")
       (define sugared (maybe-postfix-sugar expr defined-lhs))
       (if sugared
           (lower sugared base defined-lhs)
           (values expr '()))]

      [(list)
       (define nt (fresh base))
       (values nt (list (list nt '() 'eps)))]

      [(list head es ...)
       (define kind (operator-kind head defined-lhs))
       (cond
         [(eq? kind 'seq)
          (lower-seq es)]
         [(eq? kind 'or)
          (when (null? es)
            (error 'ext-parser "~or/or 至少要有一个分支"))
          (define nt (fresh base))
          (define rules '())
          (define own '())
          (for ([e es])
            (define-values (r rs) (lower e base defined-lhs))
            (set! rules (append rules rs))
            (set! own (append own (list (list nt (list r) 'or)))))
          (values nt (append rules own))]
         [(eq? kind 'opt)
          (unless (= (length es) 1)
            (error 'ext-parser "~opt/opt/? 需要且仅需要 1 个参数"))
          (define-values (r rs) (lower (car es) base defined-lhs))
          (define nt (fresh base))
          (values nt
                  (append rs
                          (list (list nt '() 'opt-empty)
                                (list nt (list r) 'opt-some))))]
         [(eq? kind 'rep0)
          (unless (= (length es) 1)
            (error 'ext-parser "~rep0/rep0/* 需要且仅需要 1 个参数"))
          (define-values (r rs) (lower (car es) base defined-lhs))
          (define nt (fresh base))
          (values nt
                  (append rs
                          (list (list nt '() 'rep0-empty)
                                (list nt (list r nt) 'rep0-step))))]
         [(eq? kind 'rep1)
          (unless (= (length es) 1)
            (error 'ext-parser "~rep1/rep1/+ 需要且仅需要 1 个参数"))
          (define-values (r rs) (lower (car es) base defined-lhs))
          (define nt* (fresh base))
          (define nt+ (fresh base))
          (values nt+
                  (append rs
                          (list (list nt* '() 'rep0-empty)
                                (list nt* (list r nt*) 'rep0-step)
                                (list nt+ (list r nt*) 'rep1-step))))]
         [else
          (when (and (symbol? head) (symbol-prefix? head "~"))
            (error 'ext-parser (format "未知扩展操作符：~s" head)))
          (lower-seq expr)])]

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
                    (lambda (old) (append old (list (list rhs action))))
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
         [(list (? symbol? lhs) _ ...)
          (ensure-plain-symbol lhs "grammar 左侧非终结符")
          lhs]
         [_ (error 'ext-parser
                   (format "grammar 规则必须是 [lhs production ...]，实际: ~s" r))]))))

  (define (normalize-production prod)
    (cond
      [(and (list? prod) (pair? prod) (list? (car prod)))
       (values (car prod) (cdr prod))]
      [else
       ;; 兼容旧写法：[lhs expr]，视为单元素 RHS，动作默认取该值
       (values (list prod) '())]))

  (define (transform-grammar-datum rules-datum empty-token-syms)
    (reset-gensym!)
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
                 (define-values (root rs) (lower item lhs defined-lhs))
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
          [((~or term NUM?)) $1]]
         [term
          [((~opt NUM) (~rep0 (~or LPAREN RPAREN))) (list $1 $2)]]])))))
