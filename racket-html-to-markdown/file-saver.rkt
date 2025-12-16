#lang racket/base

(require racket/contract
         racket/file)

;; Contract definitions
(define/contract (save-markdown content filename output-dir)
  (-> string? string? string? void?)
  "Save Markdown content to a file"
  (define output-path (build-path output-dir filename))
  
  ;; Create directory if it doesn't exist
  (when (not (directory-exists? output-dir))
    (make-directory* output-dir))
  
  ;; Save the content
  (call-with-output-file output-path
    (lambda (out)
      (display content out))
    #:exists 'replace
    #:mode 'text
    #:encoding 'utf-8)
  
  (printf "Saved Markdown to: ~a\n" (path->string output-path)))

(provide save-markdown)
