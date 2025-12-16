#lang racket/base
(require racket/contract
         racket/string
         sxml
         net/url)

;; Contract definitions
(define/contract (html-to-markdown sxml base-url)
  (-> list? string? string?)
  "Convert an SXML structure to Markdown format"
  (define (process-element elem)
    (cond
      [(string? elem)
       (escape-special-chars elem)]
      [(symbol? elem)
       (symbol->string elem)]
      [(not (list? elem))
       ""]
      [(string? (car elem))
       (case (string-downcase (car elem))
         ;; Headings
         [(h1) (format "# ~a\n\n" (process-children (cdr elem)))]
         [(h2) (format "## ~a\n\n" (process-children (cdr elem)))]
         [(h3) (format "### ~a\n\n" (process-children (cdr elem)))]
         [(h4) (format "#### ~a\n\n" (process-children (cdr elem)))]
         [(h5) (format "##### ~a\n\n" (process-children (cdr elem)))]
         [(h6) (format "###### ~a\n\n" (process-children (cdr elem)))]

         ;; Paragraph
         [(p) (format "~a\n\n" (process-children (cdr elem)))]

         ;; Lists
         [(ul) (process-unordered-list (cdr elem))]
         [(ol) (process-ordered-list (cdr elem))]
         [(li) (format "- ~a\n" (process-children (cdr elem)))]

         ;; Links
         [(a) (process-link elem base-url)]

         ;; Images
         [(img) (process-image elem base-url)]

         ;; Code
         [(pre) (process-pre elem)]
         [(code) (format "`~a`" (process-children (cdr elem)))]

         ;; Emphasis
         [(strong b) (format "**~a**" (process-children (cdr elem)))]
         [(em i) (format "*~a*" (process-children (cdr elem)))]

         ;; Blockquote
         [(blockquote) (process-blockquote (cdr elem))]

         ;; Horizontal rule
         [(hr) "---\n\n"]

         ;; Tables
         [(table) (process-table (cdr elem))]
         [(tr) (process-table-row (cdr elem))]
         [(td th) (format "| ~a " (process-children (cdr elem)))]

         ;; Other elements
         [else (process-children (cdr elem))])]
      [else (process-children elem)]))

  (define (process-children children)
    "Process a list of child elements"
    (apply string-append (map process-element children)))

  (define (process-unordered-list items)
    "Process an unordered list"
    (string-append (apply string-append (map process-element items)) "\n\n"))

  (define (process-ordered-list items)
    "Process an ordered list"
    (define (process-ordered-item item index)
      (format "~a. ~a\n" index (process-children (cdr item))))

    (string-append
     (apply string-append (for/list ([item items] [i (in-naturals 1)])
                            (process-ordered-item item i)))
     "\n\n"))

  (define (process-link elem base-url)
    "Process a link element"
    (define href (sxml:attr elem 'href ""))
    (define full-href (url->string (combine-url/relative (string->url base-url) href)))
    (define text (process-children (cdr elem)))
    (format "[~a](~a)" text full-href))

  (define (process-image elem base-url)
    "Process an image element"
    (define src (sxml:attr elem 'src ""))
    (define alt (sxml:attr elem 'alt ""))
    (define full-src (url->string (combine-url/relative (string->url base-url) src)))
    (format "![~a](~a)"
            (if (non-empty-string? alt) alt "image")
            full-src))

  (define (process-pre elem)
    "Process a preformatted text element"
    (define code-elem (sxpath '(code) elem))
    (define code (if (and code-elem (not (null? code-elem)))
                     (process-children (cdr (car code-elem)))
                     (process-children (cdr elem))))

    (define lang (extract-language-from-class elem))
    (format "```~a\n~a\n```\n\n" lang code))

  (define (extract-language-from-class elem)
    "Extract language from class attribute"
    (define class (sxml:attr elem 'class ""))
    (define classes (string-split class))

    (for/or ([c classes])
      (cond
        [(string-prefix? c "language-") (substring c 9)]
        [(string-prefix? c "lang-") (substring c 5)]
        [else #f])))

  (define (process-blockquote content)
    "Process a blockquote element"
    (define text (process-children content))
    (define lines (string-split text "\n"))
    (define quoted-lines (map (lambda (line) (if (non-empty-string? line) (string-append "> " line) line)) lines))
    (string-append (string-join quoted-lines "\n") "\n\n"))

  (define (process-table rows)
    "Process a table element"
    (define header-row #f)
    (define body-rows '())

    (for ([row rows])
      (when (and (list? row) (string? (car row)))
        (case (string-downcase (car row))
          [(thead)
           (set! header-row (process-table-header (cdr row)))]
          [(tbody)
           (set! body-rows (append body-rows (process-table-body (cdr row))))]
          [(tr)
           (if header-row
               (set! body-rows (cons row body-rows))
               (set! header-row row))])))

    (string-append
     (if header-row (process-table-row (cdr header-row)) "")
     (if header-row (make-table-separator header-row) "")
     (apply string-append (map (lambda (row) (process-table-row (cdr row))) body-rows))
     "\n\n"))

  (define (process-table-header header)
    "Process table header"
    (for/or ([elem header])
      (when (and (list? elem) (string? (car elem)) (string=? (string-downcase (car elem)) "tr"))
        elem)))

  (define (process-table-body body)
    "Process table body"
    (filter (lambda (elem) (and (list? elem) (string? (car elem)) (string=? (string-downcase (car elem)) "tr")))
            body))

  (define (process-table-row cells)
    "Process a table row"
    (string-append (apply string-append (map process-element cells)) "|\n"))

  (define (make-table-separator header-row)
    "Create a separator row for tables"
    (define cells (filter (lambda (elem) (and (list? elem) (string? (car elem)) (or (string=? (string-downcase (car elem)) "th") (string=? (string-downcase (car elem)) "td"))))
                          (cdr header-row)))

    (string-append (apply string-append (map (lambda (cell) "| --- ") cells)) "|\n"))

  (define (escape-special-chars str)
    "Escape special Markdown characters"
    (for/fold ([s str])
              ([char '(#\* #\_ #\` #\~ #\!)])
      (string-replace s (string char) (string-append "\\" (string char)))))
  
  (define markdown (process-element sxml))
  
  ;; Post-process the markdown
  (post-process-markdown markdown))

(define/contract (post-process-markdown markdown)
  (-> string? string?)
  "Post-process the markdown to fix common issues"
  (define lines (string-split markdown "\n"))
  
  ;; Fix multiple newlines
  (define cleaned-lines
    (let loop ([lines lines] [prev-empty? #f] [result '()])
      (cond
        [(null? lines) (reverse result)]
        [(not (non-empty-string? (car lines)))
         (if prev-empty?
             (loop (cdr lines) #t result)
             (loop (cdr lines) #t (cons (car lines) result)))]
        [else
         (loop (cdr lines) #f (cons (car lines) result))])))
  
  ;; Join lines and return
  (string-join cleaned-lines "\n"))

(provide html-to-markdown)
