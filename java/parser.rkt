#lang racket/base

(require racket/port
         "antlr-parser.rkt"
         "ast.rkt")

(provide java-parser
         parse-java-code
         parse-java-file
         parse-java-parse-tree
         parse-java-file-parse-tree
         build-antlr-java-parser!
         parse-java-code/antlr
         parse-java-file/antlr
         (all-from-out "ast.rkt"))

(define (parse-java-parse-tree source #:include-hidden? [include-hidden? #f])
  (parse-java-code/antlr source #:include-hidden? include-hidden?))

(define (parse-java-file-parse-tree path #:include-hidden? [include-hidden? #f])
  (parse-java-file/antlr path #:include-hidden? include-hidden?))

(define (parse-java-code source #:include-hidden? [include-hidden? #f])
  (parse-tree->ast (parse-java-parse-tree source #:include-hidden? include-hidden?)))

(define (parse-java-file path #:include-hidden? [include-hidden? #f])
  (parse-tree->ast (parse-java-file-parse-tree path #:include-hidden? include-hidden?)))

(define (java-parser input #:include-hidden? [include-hidden? #f])
  (cond
    [(string? input) (parse-java-code input #:include-hidden? include-hidden?)]
    [(input-port? input) (parse-java-code (port->string input) #:include-hidden? include-hidden?)]
    [else
     (raise-argument-error 'java-parser
                           "(or/c string? input-port?)"
                           input)]))

(module+ main
  (require racket/cmdline
           racket/pretty)

  (define raw? #f)
  (define check-only? #f)
  (define include-hidden? #f)

  (define files
    (command-line
     #:program "java/parser.rkt"
     #:once-each
     [("--raw") "Print the raw ANTLR parse tree instead of the Racket AST."
      (set! raw? #t)]
     [("--check") "Only check whether each file parses successfully."
      (set! check-only? #t)]
     [("--include-hidden") "Include tokens from ANTLR's hidden channel."
      (set! include-hidden? #t)]
     #:args paths
     paths))

  (when (null? files)
    (raise-user-error 'java/parser.rkt "expected at least one Java source file"))

  (for ([file (in-list files)])
    (define result
      (if raw?
          (parse-java-file-parse-tree file #:include-hidden? include-hidden?)
          (parse-java-file file #:include-hidden? include-hidden?)))
    (cond
      [check-only?
       (printf "~a: ok\n" file)]
      [(null? (cdr files))
       (pretty-write result)]
      [else
       (printf ";; ~a\n" file)
       (pretty-write result)])))
