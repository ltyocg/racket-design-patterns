#lang racket
(require racket/match
         racket/list
         racket/string)

;; ---------- 数据结构 ----------
(struct Prod (rhs tag) #:transparent)   ; rhs: '(A B C), tag: 用于自动动作
(struct Rule (lhs prods) #:transparent) ; prods: (listof Prod)

;; ---------- 新符号 ----------
(define counter 0)
(define (fresh base)
  (set! counter (add1 counter))
  (string->symbol (format "~a$~a" base counter)))

;; ---------- EBNF(S-Expr) -> 中间规则 ----------
;; Expr:
;;   symbol
;;   (seq e ...)
;;   (alt e ...)
;;   (opt e)
;;   (rep0 e)   ; *
;;   (rep1 e)   ; +
;;
;; 注意：rep0/rep1 这里用右递归，便于生成扁平 list 动作
(define (lower e base)
  (match e
    [(? symbol?) (values e '())]

    [`(seq ,es ...)
     (define syms '())
     (define rules '())
     (for ([x es])
       (define-values (s rs) (lower x base))
       (set! syms (append syms (list s)))
       (set! rules (append rules rs)))
     (define nt (fresh base))
     (values nt (append rules (list (Rule nt (list (Prod syms 'seq))))))]

    [`(alt ,es ...)
     (define alts '())
     (define rules '())
     (for ([x es])
       (define-values (s rs) (lower x base))
       (set! alts (append alts (list (Prod (list s) 'alt))))
       (set! rules (append rules rs)))
     (define nt (fresh base))
     (values nt (append rules (list (Rule nt alts))))]

    [`(opt ,x)
     (define-values (s rs) (lower x base))
     (define nt (fresh base))
     (values nt
             (append rs
                     (list (Rule nt (list (Prod '() 'opt-empty)
                                          (Prod (list s) 'opt-some))))))]

    [`(rep0 ,x)
     (define-values (s rs) (lower x base))
     (define nt (fresh base))
     ;; nt -> () | s nt
     (values nt
             (append rs
                     (list (Rule nt (list (Prod '() 'rep0-empty)
                                          (Prod (list s nt) 'rep0-step))))))]

    [`(rep1 ,x)
     (define-values (s rs) (lower x base))
     (define nt (fresh base))
     ;; nt -> s | s nt
     (values nt
             (append rs
                     (list (Rule nt (list (Prod (list s) 'rep1-base)
                                          (Prod (list s nt) 'rep1-step))))))]

    [_ (error 'lower "unknown expr: ~a" e)]))

;; grammar-spec: '((expr <Expr>) (term <Expr>) ...)
(define (lower-grammar grammar-spec)
  (append-map
   (lambda (r)
     (match r
       [(list lhs expr)
        (define-values (root extra) (lower expr lhs))
        ;; 顶层 lhs 仅重定向到 root，便于保留原始非终结符名
        (cons (Rule lhs (list (Prod (list root) 'redirect))) extra)]))
   grammar-spec))

;; ---------- 规则归并 ----------
(define (normalize-rules rules)
  (define h (make-hash))
  (define lhs-order '())
  (for ([r rules])
    (define lhs (Rule-lhs r))
    (unless (member lhs lhs-order)
      (set! lhs-order (append lhs-order (list lhs))))
    (hash-update! h lhs
                  (lambda (old) (append old (Rule-prods r)))
                  '()))
  (for/list ([lhs lhs-order])
    (Rule lhs (hash-ref h lhs))))

;; ---------- 自动语义动作 ----------
;; 可用 overrides 覆盖动作：key = (cons lhs rhs), value = "动作字符串"
(define (default-action prod)
  (define rhs (Prod-rhs prod))
  (match (Prod-tag prod)
    ['rep0-empty "'()"]
    ['rep0-step "(cons $1 $2)"]
    ['rep1-base "(list $1)"]
    ['rep1-step "(cons $1 $2)"]
    ['opt-empty "#f"]   ; 也可改成 "'()"
    ['opt-some "$1"]
    [_ (cond
         [(null? rhs) "'()"]
         [(= (length rhs) 1) "$1"]
         [else
          (format "(list ~a)"
                  (string-join
                   (for/list ([i (in-range 1 (add1 (length rhs)))])
                     (format "$~a" i))
                   " "))])]))

(define (rhs->text rhs)
  (if (null? rhs)
      "()"
      (format "(~a)" (string-join (map symbol->string rhs) " "))))

(define (emit-grammar rules #:action-overrides [action-overrides (hash)] #:indent [indent "    "])
  (define norm (normalize-rules rules))
  (string-append
   "(grammar\n"
   (string-join
    (for/list ([r norm])
      (define lhs (Rule-lhs r))
      (define prods (Rule-prods r))
      (string-append
       indent (format "(~a\n" lhs)
       (string-join
        (for/list ([p prods])
          (define rhs (Prod-rhs p))
          (define action
            (hash-ref action-overrides
                      (cons lhs rhs)
                      (default-action p)))
          (format "~a  (~a ~a)" indent (rhs->text rhs) action))
        "\n")
       (format "\n~a)" indent)))
    "\n")
   "\n  )"))

;; ---------- 输出完整 parser 骨架 ----------
(define (emit-parser-skeleton
         rules
         #:parser-name [parser-name 'parse]
         #:start [start 'expr]
         #:end [end 'EOF]
         #:token-groups [token-groups '(value-tokens op-tokens)]
         #:src-pos? [src-pos? #t]
         #:action-overrides [action-overrides (hash)])
  (define grammar-text
    (emit-grammar rules #:action-overrides action-overrides #:indent "    "))
  (string-append
   "#lang racket\n"
   "(require parser-tools/yacc)\n\n"
   (format "(define ~a\n" parser-name)
   "  (parser\n"
   (format "    (start ~a)\n" start)
   (format "    (end ~a)\n" end)
   (format "    (tokens ~a)\n"
           (string-join (map symbol->string token-groups) " "))
   (if src-pos? "    (src-pos)\n" "")
   (format "    (error (lambda args (error '~a (format \"parse error: ~a\" args))))\n"
           parser-name "~s")
   "    "
   grammar-text
   "\n"
   "  ))\n"))

;; ---------- 示例 ----------
(module+ main
  (define ebnf
    '((expr (seq term (rep0 (seq PLUS term))))
      (term (alt NUM (seq LPAREN expr RPAREN)))))

  (define rules (lower-grammar ebnf))

  ;; 覆盖具体产生式动作（可选）
  ;; 例如 expr -> term expr_tail 时，想做 fold 运算可以在这里替换
  (define overrides
    (hash
     ;; (cons 'expr '(expr$1)) "$1" ; 按需覆盖
     ))

  (displayln (emit-parser-skeleton rules
                                   #:parser-name 'parse-expr
                                   #:start 'expr
                                   #:end 'EOF
                                   #:token-groups '(value-tokens op-tokens)
                                   #:action-overrides overrides)))