#lang racket/base

(require racket/contract
         racket/function
         racket/list
         racket/match
         racket/string
         html-parsing
         sxml
         sxml/xpath
         net/url)

;; Contract definitions
(define/contract (parse-html html-string)
  (-> string? list?)
  "Parse an HTML string into an SXML structure"
  (html->xexpr html-string))

(define/contract (extract-main-content sxml base-url)
  (-> list? string? (or/c list? #f))
  "Extract the main content from an SXML structure"
  (define (remove-unwanted-elements sxml)
    "Remove unwanted elements from the SXML structure"
    (define (process-element elem)
      (cond
        [(not (list? elem)) elem]
        [(string? (car elem))
         (case (string-downcase (car elem))
           [(script noscript style iframe frame nav header footer)
            ;; Remove these elements entirely
            '()]
           [(div span section article main)
            ;; Process these elements and their children
            (cons (car elem) 
                  (filter (lambda (x) (not (equal? x '()))) 
                          (map process-element (cdr elem))))]
           [else
            ;; Process other elements normally
            (cons (car elem) (map process-element (cdr elem)))])]
        [else
         ;; Process non-element nodes
         (map process-element elem)]))
    
    (process-element sxml))
  
  (define (find-main-content sxml)
    "Find the main content using common selectors"
    (or
     ;; Try common main content selectors
     (sxpath '(// main) sxml)
     (sxpath '(// div (@ (contains? @class "main-content"))) sxml)
     (sxpath '(// div (@ (contains? @class "document"))) sxml)
     (sxpath '(// div (@ (contains? @id "main-content"))) sxml)
     (sxpath '(// div (@ (contains? @id "content"))) sxml)
     (sxpath '(// article) sxml)
     (sxpath '(// section) sxml)
     ;; If no specific selectors found, use body content
     (let ([body (sxpath '(// body) sxml)])
       (if body
           (list (list 'body (cdr (car body))))
           #f))))
  
  (define cleaned-sxml (remove-unwanted-elements sxml))
  (define main-content (find-main-content cleaned-sxml))
  
  (if (and main-content (not (null? main-content)))
      (if (list? (car main-content))
          (car main-content)
          main-content)
      #f))

(provide parse-html extract-main-content)
