#lang racket/base

(require rackunit
         parser-tools/lex
         "lexer.rkt"
         "parser.rkt")

(define (parse-ok source)
  (check-not-exn (lambda () (parse-java-code source))))

(define (parse-fails source)
  (check-exn exn:fail? (lambda () (parse-java-code source))))

(define (token-name* tok)
  (define raw (position-token-token tok))
  (if (token? raw) (token-name raw) raw))

(define (tokenize-source source)
  (define input (open-input-string source))
  (let loop ([tokens '()])
    (define tok (java-lexer input))
    (if (eq? (token-name* tok) 'EOF)
        (reverse tokens)
        (loop (cons tok tokens)))))

(module+ test
  (parse-ok "@ interface A {}")
  (parse-ok "import java.util.*; public class C {}")
  (parse-ok "module m { requires java.base; exports a.b; }")

  (parse-ok "class C { void m() { String[] s; int[][] n = new int[2][]; } }")
  (parse-ok "class C { void m() { List<Integer> xs = new ArrayList<>(); for (Name n : names) {} for (int i = 0; i < 10; i++) {} } }")
  (parse-ok "class C { void m() throws Exception { try (var a = new AC()) {} } }")
  (parse-ok "class C { void m() { x = (DoubleConsumer) action::accept; result = (a < b) ? a : b; } }")

  (parse-ok "class C { Object m(E e) { return switch (e) { case ONE -> 0; }; } }")
  (parse-ok "class C { void m(Object o) { switch (o) { case String s -> f(1); case int[] a -> f(2); default -> f(-1); } } }")
  (parse-ok "class C { void m(Object n) { if (n instanceof @A final Long l) ; } }")

  (parse-ok "class C { static <T> org.host.test.@N Bar<T> fn1(org.host.test.@N Bar<T> p) { return null; } }")
  (parse-ok "class C { void b(Issue1897.C.@Dum3 D Issue1897.C.D.this) {} }")
  (parse-ok "class C { void h(){ SS.Sup<provides<Long>.with<Long>> s = @Issue1897.Dum1 provides<Long>.with<Long>::new; } }")

  (parse-fails "class C { var f; }")
  (parse-fails "class C { void m() { var x; } }")
  (parse-fails "class C { void m() { + + ; } }")

  (let ([tokens (tokenize-source "class C { /* first */ int x; /* second */ }")])
    (check-equal? (length (filter (lambda (tok)
                                    (eq? (token-name* tok) 'COMMENT))
                                  tokens))
                  2)))
