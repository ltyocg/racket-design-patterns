#lang racket

(require "antlr/lexer.rkt"
         "antlr/parser.rkt"
         parser-tools/lex
         racket/list
         net/url)

;; 从 URL 获取内容
(define (fetch-url str)
  (define u (string->url str))
  (define port (get-pure-port u))
  (define content (port->string port))
  (close-input-port port)
  content)

;; 对字符串进行词法分析，返回 token 列表
(define (lex-content content)
  (define port (open-input-string content))
  (port-count-lines! port)  ;; 启用行号追踪
  (let loop ([tokens '()])
    (define tok (antlr4-lexer port))
    (define t (position-token-token tok))
    (define name (if (token? t) (token-name t) t))
    (cond
      [(eq? name 'EOF)
       (reverse (cons tok tokens))]
      [(or (eq? name 'UNTERMINATED_STRING_LITERAL)
           (eq? name 'UNTERMINATED_ARGUMENT)
           (eq? name 'UNTERMINATED_CHAR_SET))
       (printf "Warning: ~a at ~a\n" name (format-token tok))
       (reverse (cons tok tokens))]
      [else
       (loop (cons tok tokens))])))

;; 格式化输出单个 token
(define (format-token pt)
  (define t (position-token-token pt))
  (define name (if (token? t) (token-name t) t))
  (define value (if (token? t) (token-value t) t))
  (define start-pos (position-token-start-pos pt))
  (define line (position-line start-pos))
  (define col (position-col start-pos))
  (format "~a:~a: ~a ~v" line col name value))

;; 格式化 AST 输出
(define (format-ast ast [indent 0])
  (define indent-str (make-string (* indent 2) #\space))
  (cond
    [(struct? ast)
     (define fields (cdr (vector->list (struct->vector ast))))
     (format "~a~a~a\n"
             indent-str
             (object-name ast)
             (if (null? fields)
                 ""
                 (string-append
                  "\n"
                  (string-join
                   (map (lambda (v) (format-ast v (+ indent 1))) fields)
                   ""))))]
    [(list? ast)
     (if (null? ast)
         (format "~a[]\n" indent-str)
         (string-append
          (format "~a[\n" indent-str)
          (string-join (map (lambda (v) (format-ast v (+ indent 1))) ast) "")
          (format "~a]\n" indent-str)))]
    [else (format "~a~v\n" indent-str ast)]))

;; 主函数
(define (main)
  ;; 测试简单的语法
  (define test-grammar
    "grammar Expr;

     expr: term (('+' | '-') term)* ;
     term: factor (('*' | '/') factor)* ;
     factor: NUMBER | '(' expr ')' ;

     NUMBER: [0-9]+ ;
     WS: [ \\t\\r\\n]+ ;
     ")

  (printf "=== Testing with simple grammar ===\n\n")
  (printf "Grammar:\n~a\n\n" test-grammar)

  ;; 词法分析
  (printf "=== Lexing ===\n\n")
  (define tokens (lex-content test-grammar))
  (printf "Total ~a tokens\n\n" (length tokens))

  ;; 显示所有 token
  (printf "All tokens:\n")
  (for ([tok tokens]
        [i (in-naturals 1)])
    (printf "~a. ~a\n" i (format-token tok)))
  (printf "\n")

  ;; 语法分析
  (printf "=== Parsing ===\n\n")
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "✗ Parse error: ~a\n" (exn-message e)))])
    (define ast (parse-string test-grammar))
    (printf "✓ Parse successful!\n\n")
    (printf "Grammar details:\n")
    (printf "  Type: ~a\n" (grammar-spec-type ast))
    (printf "  Name: ~a\n" (grammar-spec-name ast))
    (printf "  Prequels: ~a\n" (length (grammar-spec-prequels ast)))
    (printf "  Rules: ~a\n" (length (grammar-spec-rules ast)))
    (printf "  Modes: ~a\n" (length (grammar-spec-modes ast)))

    (printf "\nAST structure:\n")
    (printf "~a" (format-ast ast)))

  ;; 可选：从 URL 获取真实的 ANTLR 语法
  (printf "\n\n=== Fetching real ANTLR grammar (optional) ===\n\n")
  (define url "https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr/antlr4/ANTLRv4Lexer.g4")
  (printf "URL: ~a\n" url)
  (printf "Note: This is a complex grammar that may not fully parse.\n")
  (printf "Uncomment the code below to test with it.\n")

  #|
  (printf "Fetching...\n")
  (define content (fetch-url url))
  (printf "Content length: ~a bytes\n\n" (string-length content))
  (printf "Lexing...\n")
  (define tokens2 (lex-content content))
  (printf "Total ~a tokens\n\n" (length tokens2))
  (printf "Parsing...\n")
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "Parse error: ~a\n" (exn-message e)))])
    (define ast2 (parse-string content))
    (printf "Parse successful!\n"))
  |#
  )

(main)
