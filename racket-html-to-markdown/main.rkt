#lang racket/base

(require racket/cmdline
         racket/contract
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/port
         racket/string
         "url-fetcher.rkt"
         "html-parser.rkt"
         "html-to-markdown.rkt"
         "file-saver.rkt")

;; Main function
(define (main args)
  (define url #f)
  (define output-file "output.md")
  (define output-dir "output")
  (define verbose? #f)
  
  (command-line
   #:program "racket-html-to-markdown"
   #:once-each
   [("-o" "--output") file "Output Markdown file path (default: output.md)"
    (set! output-file file)]
   [("-d" "--output-dir") dir "Directory to save output files (default: output)"
    (set! output-dir dir)]
   [("-v" "--verbose") "Enable verbose logging"
    (set! verbose? #t)]
   #:args (url-arg)
   (set! url url-arg))
  
  (when verbose?
    (printf "Starting conversion of ~a\n" url)
    (printf "Output will be saved to ~a\n" (build-path output-dir output-file)))
  
  (with-handlers ([exn:fail? (lambda (e)
                               (eprintf "Error: ~a\n" (exn-message e))
                               (exit 1))])
    ;; Fetch the URL
    (when verbose? (printf "Fetching URL: ~a\n" url))
    (define html-content (fetch-url url))
    
    (when (string-empty? html-content)
      (error "Empty response from URL"))
    
    ;; Parse HTML
    (when verbose? (printf "Parsing HTML...\n"))
    (define sxml (parse-html html-content))
    
    ;; Extract main content
    (when verbose? (printf "Extracting main content...\n"))
    (define main-content (extract-main-content sxml url))
    
    (when (not main-content)
      (error "Could not find main content in the HTML"))
    
    ;; Convert to Markdown
    (when verbose? (printf "Converting to Markdown...\n"))
    (define markdown (html-to-markdown main-content url))
    
    ;; Add header with source information
    (define header (format "# Converted from ~a\n\nThis document was automatically converted from the original web page.\nFor the most up-to-date version, please visit the original site.\n\n" url))
    (define full-markdown (string-append header markdown))
    
    ;; Save to file
    (when verbose? (printf "Saving to file...\n"))
    (save-markdown full-markdown output-file output-dir)
    
    (printf "Conversion completed successfully!\n")
    (printf "Output saved to: ~a\n" (build-path output-dir output-file))
    (exit 0)))

;; Run the main function
(module+ main
  (main (current-command-line-arguments)))
