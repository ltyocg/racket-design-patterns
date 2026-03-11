#lang racket/base
(require racket/class)
(define document<%>
  (interface () put get children))