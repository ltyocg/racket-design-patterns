#lang typed/racket
;; Abstract Document — Racket-style record + view (NO classes, NO interfaces)
;; 核心思想：
;;  - 数据 = 纯 record（不可变）
;;  - 语义 = view（一组带类型的函数）
;;  - 组合 = 函数组合，而不是继承 / implements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key & value domain (same discipline as before)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type DocKey (U 'model 'price 'parts 'type))

(define-type Scalar (U String Number))

(define-type DocValue
  (U Scalar
     DocProps
     (Listof DocProps)))

(define-type DocProps (Immutable-HashTable DocKey DocValue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct document ([props : DocProps]) #:transparent)

(: doc-get (document DocKey -> (Option DocValue)))
(define (doc-get d k)
  (hash-ref (document-props d) k #f))

(: doc-put (document DocKey DocValue -> document))
(define (doc-put d k v)
  (document (hash-set (document-props d) k v)))

(: doc-children
   (document DocKey (DocProps -> document)
             -> (Listof document)))
(define (doc-children d k ctor)
  (match (doc-get d k)
    [(? false?) '()]
    [(? hash? h) (list (ctor h))]
    [(list hs ...) (map ctor hs)]
    [_ '()]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Views (semantic accessors)
;; —— 这就是 HasX interface 的函数化版本
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HasModel
(: view-model (document -> (Option String)))
(define (view-model d)
  (match (doc-get d 'model)
    [(? string? s) s]
    [_ #f]))

;; HasPrice
(: view-price (document -> (Option Number)))
(define (view-price d)
  (match (doc-get d 'price)
    [(? number? n) n]
    [_ #f]))

;; HasType
(: view-type (document -> (Option String)))
(define (view-type d)
  (match (doc-get d 'type)
    [(? string? s) s]
    [_ #f]))

;; HasParts
(: view-parts (document -> (Listof document)))
(define (view-parts d)
  (doc-children d 'parts document))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Domain-specific views (Car / Part)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Car = Document + (model, price, parts)
(: car-model (document -> (Option String)))
(define car-model view-model)

(: car-price (document -> (Option Number)))
(define car-price view-price)

(: car-parts (document -> (Listof document)))
(define car-parts view-parts)

;; Part = Document + (type, model, price)
(: part-type (document -> (Option String)))
(define part-type view-type)

(: part-model (document -> (Option String)))
(define part-model view-model)

(: part-price (document -> (Option Number)))
(define part-price view-price)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wheel-props
  (hash 'type "wheel" 'model "15C" 'price 100))

(define door-props
  (hash 'type "door" 'model "Lambo" 'price 300))

(define car-props
  (hash 'model "300SL"
        'price 10000
        'parts (list wheel-props door-props)))

(define car (document car-props))

(printf "model: ~a\n" (car-model car))
(printf "price: ~a\n" (car-price car))
(printf "parts:\n")

(for-each
 (λ ([p : document])
   (printf "\t~a / ~a / ~a\n"
           (part-type p)
           (part-model p)
           (part-price p)))
 (car-parts car))

(define car2 (doc-put car 'price 12000))
(printf "after price change -> price: ~a (original: ~a)\n"
        (car-price car2)
        (car-price car))
