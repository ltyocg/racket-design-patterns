#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

(define-runtime-path antlr-dir ".")

(define antlr-version "4.13.2")
(define jar-url
  (format "https://www.antlr.org/download/antlr-~a-complete.jar" antlr-version))
(define cache-dir (build-path antlr-dir "cache"))
(define grammar-dir (build-path antlr-dir "grammar"))
(define src-dir (build-path antlr-dir "src"))
(define build-dir (build-path antlr-dir "build"))
(define generated-dir (build-path build-dir "generated"))
(define classes-dir (build-path build-dir "classes"))
(define antlr-jar (build-path cache-dir (format "antlr-~a-complete.jar" antlr-version)))

(define (run command . args)
  (define executable
    (or (find-executable-path command)
        (error 'antlr-build "cannot find executable: ~a" command)))
  (define exit-code (apply system*/exit-code executable args))
  (unless (zero? exit-code)
    (error 'antlr-build "~a failed with exit code ~a" command exit-code)))

(define (download-antlr-jar!)
  (make-directory* cache-dir)
  (unless (file-exists? antlr-jar)
    (printf "Downloading ~a\n" jar-url)
    (run "curl" "-fsSL" jar-url "-o" (path->string antlr-jar))))

(define (java-sources dir)
  (for/list ([path (in-list (find-files (lambda (path)
                                          (string-suffix? (path->string path) ".java"))
                                        dir))])
    (path->string path)))

(define (generate-parser!)
  (delete-directory/files generated-dir #:must-exist? #f)
  (make-directory* generated-dir)
  (parameterize ([current-directory grammar-dir])
    (run "java"
         "-jar"
         (path->string antlr-jar)
         "-visitor"
         "-no-listener"
         "-Xexact-output-dir"
         "-o"
         (path->string generated-dir)
         "JavaLexer.g4"
         "JavaParser.g4")))

(define (compile-parser!)
  (delete-directory/files classes-dir #:must-exist? #f)
  (make-directory* classes-dir)
  (define sources (append (java-sources generated-dir) (java-sources src-dir)))
  (when (null? sources)
    (error 'antlr-build "no Java sources found"))
  (apply run
         "javac"
         "-cp"
         (path->string antlr-jar)
         "-d"
         (path->string classes-dir)
         sources))

(module+ main
  (download-antlr-jar!)
  (generate-parser!)
  (compile-parser!)
  (printf "ANTLR Java parser built in ~a\n" (path->string classes-dir)))
