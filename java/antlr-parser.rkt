#lang racket/base

(require racket/file
         racket/path
         racket/runtime-path
         racket/string)

(provide build-antlr-java-parser!
         parse-java-code/antlr
         parse-java-file/antlr)

(define-runtime-path antlr-dir "antlr")
(define build-script (build-path antlr-dir "build.rkt"))
(define build-dir (build-path antlr-dir "build"))
(define classes-dir (build-path antlr-dir "build" "classes"))
(define build-lock-dir (build-path build-dir ".build.lock"))
(define antlr-jar (build-path antlr-dir "cache" "antlr-4.13.2-complete.jar"))
(define parse-java-class (build-path classes-dir "ParseJava.class"))
(define build-inputs
  (list build-script
        (build-path antlr-dir "grammar" "JavaLexer.g4")
        (build-path antlr-dir "grammar" "JavaParser.g4")
        (build-path antlr-dir "src" "JavaParserBase.java")
        (build-path antlr-dir "src" "ParseJava.java")))

(define classpath-separator
  (if (eq? (system-type 'os) 'windows) ";" ":"))

(define (classpath)
  (string-join (map path->string (list classes-dir antlr-jar)) classpath-separator))

(define (run/capture command args #:stdin [input #f])
  (define executable
    (or (find-executable-path command)
        (error 'parse-java/antlr "cannot find executable: ~a" command)))
  (define stdout-path (make-temporary-file "antlr-java-stdout-~a"))
  (define stderr-path (make-temporary-file "antlr-java-stderr-~a"))
  (define status
    (call-with-output-file stdout-path
      #:exists 'truncate
      (lambda (stdout)
        (call-with-output-file stderr-path
          #:exists 'truncate
          (lambda (stderr)
            (define-values (proc _stdout stdin _stderr)
              (apply subprocess stdout #f stderr executable args))
            (when input
              (display input stdin))
            (close-output-port stdin)
            (subprocess-wait proc)
            (subprocess-status proc))))))
  (define stdout (file->string stdout-path))
  (define stderr (file->string stderr-path))
  (delete-file stdout-path)
  (delete-file stderr-path)
  (values status stdout stderr))

(define (build-antlr-java-parser!)
  (define racket-bin (or (find-executable-path "racket")
                         (error 'build-antlr-java-parser! "cannot find racket executable")))
  (define-values (status stdout stderr)
    (run/capture (path->string racket-bin) (list (path->string build-script))))
  (unless (zero? status)
    (error 'build-antlr-java-parser!
           "build failed with exit code ~a\n~a~a"
           status
           stdout
           stderr)))

(define (parser-outdated?)
  (or (not (file-exists? parse-java-class))
      (for/or ([input-path (in-list build-inputs)])
        (and (file-exists? input-path)
             (> (file-or-directory-modify-seconds input-path)
                (file-or-directory-modify-seconds parse-java-class))))))

(define (acquire-build-lock!)
  (make-directory* build-dir)
  (let loop ([remaining-attempts 1200])
    (with-handlers ([exn:fail:filesystem?
                     (lambda (_exn)
                       (when (zero? remaining-attempts)
                         (error 'build-antlr-java-parser!
                                "timed out waiting for build lock: ~a"
                                (path->string build-lock-dir)))
                       (sleep 0.1)
                       (loop (sub1 remaining-attempts)))])
      (make-directory build-lock-dir))))

(define (release-build-lock!)
  (with-handlers ([exn:fail:filesystem? void])
    (delete-directory build-lock-dir)))

(define (with-build-lock thunk)
  (acquire-build-lock!)
  (dynamic-wind void thunk release-build-lock!))

(define (ensure-built!)
  (when (parser-outdated?)
    (with-build-lock
     (lambda ()
       ;; Another process may have completed the rebuild while this one waited.
       (when (parser-outdated?)
         (build-antlr-java-parser!))))))

(define (parse-output stdout)
  (define in (open-input-string stdout))
  (read in))

(define (run-parser args #:stdin [input #f])
  (ensure-built!)
  (define-values (status stdout stderr)
    (run/capture "java" (append (list "-cp" (classpath) "ParseJava") args) #:stdin input))
  (unless (zero? status)
    (error 'parse-java/antlr
           "ANTLR parser failed with exit code ~a\n~a"
           status
           stderr))
  (parse-output stdout))

(define (parser-options include-hidden?)
  (if include-hidden? '("--include-hidden") '()))

(define (parse-java-file/antlr path #:include-hidden? [include-hidden? #f])
  (run-parser (append (parser-options include-hidden?)
                      (list (path->string (simple-form-path path))))))

(define (parse-java-code/antlr source #:include-hidden? [include-hidden? #f])
  (run-parser (append (parser-options include-hidden?) (list "--stdin")) #:stdin source))
