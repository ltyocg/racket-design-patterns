#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (for-syntax racket/base
                     racket/match
                     syntax/parse
                     parser-tools/private-lex/token-syntax))

(begin-for-syntax
  (define gensym-counter 0)
  (define (reset-gensym!)
    (set! gensym-counter 0))
  (define (fresh base)
    (set! gensym-counter (add1 gensym-counter))
    (string->symbol (format "~a$~a" base gensym-counter)))
  (define (d$ n)
    (string->symbol (format "$~a" n)))

  ;; triple = (list lhs rhs tag)
  (define (lower expr base)
    (match expr
      [(? symbol?) (values expr '())]

      [`(seq ,es ...)
       (define roots '())
       (define rules '())
       (for ([e es])
         (define-values (r rs) (lower e base))
         (set! roots (append roots (list r)))
         (set! rules (append rules rs)))
       (define nt (fresh base))
       (values nt
               (append rules
                       (list (list nt roots 'seq))))]

      [`(alt ,es ...) (lower `(or ,@es) base)]
      [`(or ,es ...)
       (define nt (fresh base))
       (define rules '())
       (define own '())
       (for ([e es])
         (define-values (r rs) (lower e base))
         (set! rules (append rules rs))
         (set! own (append own (list (list nt (list r) 'alt)))))
       (values nt (append rules own))]

      [`(opt ,e) (lower `(? ,e) base)]
      [`(? ,e)
       (define-values (r rs) (lower e base))
       (define nt (fresh base))
       (values nt
               (append rs
                       (list (list nt '() 'opt-empty)
                             (list nt (list r) 'opt-some))))]

      [`(rep ,e) (lower `(* ,e) base)]
      [`(rep0 ,e) (lower `(* ,e) base)]
      [`(* ,e)
       (define-values (r rs) (lower e base))
       (define nt (fresh base))
       ;; 右递归：扁平 list
       ;; nt -> ε | r nt
       (values nt
               (append rs
                       (list (list nt '() 'rep0-empty)
                             (list nt (list r nt) 'rep0-step))))]

      [`(rep1 ,e) (lower `(+ ,e) base)]
      [`(+ ,e)
       (define-values (r rs) (lower e base))
       (define nt (fresh base))
       ;; nt -> r | r nt
       (values nt
               (append rs
                       (list (list nt (list r) 'rep1-base)
                             (list nt (list r nt) 'rep1-step))))]

      [`(eps)
       (define nt (fresh base))
       (values nt (list (list nt '() 'eps)))]

      ;; 未识别列表默认按 seq 处理，方便写 (A B C)
      [(list es ...)
       (lower `(seq ,@es) base)]

      [_ (error 'ebnf-parser (format "不支持的 EBNF 表达式: ~s" expr))]))

  (define (parse-top-rule r)
    (match r
      [(list lhs expr) (values lhs expr)]
      [(list lhs '= expr) (values lhs expr)]
      [(list lhs '::= expr) (values lhs expr)]
      [_ (error 'ebnf-parser
                (format "grammar 规则必须是 [lhs expr] / [lhs = expr] / [lhs ::= expr]，实际: ~s" r))]))

  (define (lower-grammar rules-datum)
    (reset-gensym!)
    (define out '())
    (for ([r rules-datum])
      (define-values (lhs expr) (parse-top-rule r))
      (unless (symbol? lhs)
        (error 'ebnf-parser (format "lhs 必须是符号，实际: ~s" lhs)))
      (define-values (root extra) (lower expr lhs))
      ;; 顶层重定向，保留用户写的 lhs 名字
      (set! out (append out extra (list (list lhs (list root) 'redirect)))))
    out)

  (define (collect-empty-terminals clauses)
    (define token-groups
      (for*/list ([c clauses]
                  [ids (in-value
                        (syntax-case c (tokens)
                          [(tokens def ...) (syntax->list #'(def ...))]
                          [_ '()]))]
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

  (define (action-for tag rhs empty-token-syms)
    (define idxs (accessible-indexes rhs empty-token-syms))
    (match tag
      ['opt-empty #f]
      ['opt-some (if (null? idxs) #f (d$ (car idxs)))]
      ['rep0-empty '(quote ())]
      ['rep0-step
       (cond
         [(member 1 idxs) '(cons $1 $2)]
         [(member 2 idxs) '$2]
         [else '(quote ())])]
      ['rep1-base
       (if (member 1 idxs)
           '(list $1)
           '(quote ()))]
      ['rep1-step
       (cond
         [(member 1 idxs) '(cons $1 $2)]
         [(member 2 idxs) '$2]
         [else '(quote ())])]
      ['eps '(quote ())]
      [_ (pick-action-from-indexes idxs)]))

  (define (triples->grammar-datum triples empty-token-syms)
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
    `(grammar
      ,@(for/list ([lhs order])
          `(,lhs
            ,@(for/list ([ra (hash-ref tbl lhs)])
                (match-define (list rhs action) ra)
                `(,(if (null? rhs) '() rhs) ,action))))))

  (define (transform-clause c empty-token-syms)
    (syntax-case c (grammar)
      [(grammar rule ...)
       (let ([rules-datum (syntax->datum #'(rule ...))])
         (datum->syntax c
                        (triples->grammar-datum
                         (lower-grammar rules-datum)
                         empty-token-syms)
                        c c))]
      [_ c])))

(define-syntax (ebnf-parser stx)
  (syntax-parse stx
    [(_ clause ...+)
     (define clauses (syntax->list #'(clause ...)))
     (define empty-token-syms (collect-empty-terminals clauses))
     (define new-clauses
       (for/list ([c clauses])
         (syntax-local-introduce (transform-clause c empty-token-syms))))
     #`(parser #,@new-clauses)]))

(provide ebnf-parser)

(module+ test
  (require racket/pretty)
  (define-tokens value-tokens (NUM))
  (define-empty-tokens op-tokens (PLUS LPAREN RPAREN EOF))
  (pretty-display
   (syntax->datum
    (expand-once
     #'(ebnf-parser
        [start expr]
        [end EOF]
        [error (lambda args (error 'my-parser (format "parse error: ~s" args)))]
        [src-pos]
        [tokens value-tokens op-tokens]
        [grammar
         [expr (seq term (* (seq PLUS term)))]
         [term (or NUM (seq LPAREN expr RPAREN))]])))))
