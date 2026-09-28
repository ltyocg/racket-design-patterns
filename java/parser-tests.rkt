#lang racket/base

(require rackunit
         "parser.rkt")

(define (parse-ok source)
  (define ast (parse-java-code source))
  (check-true (ast-node? ast))
  (check-equal? (ast-node-kind ast) 'compilationunit))

(define (parse-fails source)
  (check-exn exn:fail? (lambda () (parse-java-code source))))

(module+ test
  (check-equal? (car (parse-java-parse-tree "class Raw {}"))
                'compilationunit)
  (check-equal? (ast-node-kind (java-parser "class FromString {}"))
                'compilationunit)
  (check-equal? (ast-node-kind (java-parser (open-input-string "class FromPort {}")))
                'compilationunit)

  (define comment-source "class C { /* block */ // line\n int x; }")
  (define default-ast (parse-java-code comment-source))
  (check-false (ast-find-kind 'COMMENT default-ast))
  (check-false (ast-find-kind 'LINE_COMMENT default-ast))

  (define hidden-ast (parse-java-code comment-source #:include-hidden? #t))
  (check-equal? (map ast-token-text (ast-filter-kind 'COMMENT hidden-ast))
                '("/* block */"))
  (check-equal? (map ast-token-channel (ast-filter-kind 'COMMENT hidden-ast))
                '(HIDDEN))
  (check-equal? (map ast-token-text (ast-filter-kind 'LINE_COMMENT hidden-ast))
                '("// line"))
  (check-not-false (ast-find-kind 'WS hidden-ast))
  (check-not-false (member "/* block */" (ast-token-texts hidden-ast)))
  (check-false (member "/* block */"
                       (ast-token-texts hidden-ast #:include-hidden? #f)))
  (check-equal? (car (parse-java-parse-tree comment-source #:include-hidden? #t))
                'compilationunit)

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
  (parse-fails "class C { void m() { + + ; } }"))
