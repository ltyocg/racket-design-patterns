#lang racket/base
(require racket/contract
         racket/string
         net/url
         net/http-client
         net/uri-codec)

;; Contract contract definitions
(define/contract (fetch-url url-string)
  (-> string? string?)
  "Fetch the content of a URL and return it as a string"
  (define url (string->url url-string))

  (define host (url-host url))
  (define path (url->path/param url))
  (define port (or (url-port url) 80))

  (define user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")

  (define response-port (open-output-string))
  (define status-code #f)

  (with-handlers ([exn:fail? (lambda (e)
                               (error (format "Failed to fetch URL ~a: ~a" url-string (exn-message e))))])
    (http-sendrecv host path
                   #:port port
                   #:ssl? (equal? (url-scheme url) "https")
                   #:version "1.1"
                   #:headers (list (format "Host: ~a" host)
                                   (format "User-Agent: ~a" user-agent)
                                   "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                                   "Accept-Language: en-US,en;q=0.5"
                                   "Connection: close")
                   #:response-port response-port
                   #:status-code (lambda (code) (set! status-code code))))

  (define response (get-output-string response-port))

  (unless (and status-code (between? status-code 200 299))
    (error (format "HTTP request failed with status code ~a for URL ~a" status-code url-string)))

  response)

;; Helper function to convert URL to path and parameters
(define (url->path/param url)
  (define path (url-path url))
  (define query (url-query url))

  (define path-str
    (if (null? path)
        "/"
        (string-join (map (lambda (p) (uri-encode p)) path) "/")))

  (define query-str
    (if (null? query)
        ""
        (format "?~a" (string-join (map (lambda (q) (format "~a=~a" (uri-encode (car q)) (uri-encode (cdr q)))) query) "&"))))

  (string-append path-str query-str))

;; Helper function to check if a number is between two values (inclusive)
(define (between? n min max)
  (and (>= n min) (<= n max)))

(provide fetch-url)
