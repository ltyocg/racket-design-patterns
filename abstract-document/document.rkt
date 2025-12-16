#lang typed/racket
;; Abstract Document pattern — class + interface version (NO unsafe-coerce)
;; 目标：
;; 1. 完全移除 unsafe-coerce
;; 2. 使用 Typed Racket 的 interface / class* 来模拟 Java 接口组合
;; 3. 在类型层面约束 props 的 shape，而不是事后强转

(require typed/racket/class)
(require racket/hash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key & value domain types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 所有允许的 key（对应 Java 示例中的 Property enum）
(define-type DocKey (U 'model 'price 'parts 'type))

;; 基础值类型（不允许随意 Any）
(define-type Scalar (U String Number))

;; props 中 value 的合法形状
;; - Scalar
;; - 子 document 的 props（递归）
;; - 子 document props 的列表
(define-type DocValue
  (U Scalar
     DocProps
     (Listof DocProps)))

(define-type DocProps (Immutable-HashTable DocKey DocValue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base interface: Document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Document<%>
  (Interface
   (get (-> DocKey (Option DocValue)))
   (put! (-> DocKey DocValue Void))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; document% : AbstractDocument
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define document%
  (class* object% (Document<%>)
    (init-field [init-props : DocProps])
    (super-new)

    ;; 可变字段，类型仍然受 DocProps 约束
    (define props : (Mutable-HashTable DocKey DocValue)
      (hash-copy init-props))

    (: get (-> DocKey (Option DocValue)))
    (define/public (get k)
      (hash-ref props k #f))

    (: put! (-> DocKey DocValue Void))
    (define/public (put! k v)
      (hash-set! props k v))

    ;; 受类型保护的 children
    (: children (-> DocKey (-> DocProps Document<%>) (Listof Document<%>)))
    (define/public (children k ctor)
      (match (get k)
        [(? false?) '()]
        [(? hash? h) (list (ctor h))]
        [(list hs ...) (map ctor hs)]
        [_ '()]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic interfaces (traits)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type HasModel<%>
  (Class (get-model (-> (Option String)))))

(define-type HasPrice<%>
  (Class (get-price (-> (Option Number)))))

(define-type HasType<%>
  (Class (get-type (-> (Option String)))))

(define-type HasParts<%>
  (Class (get-parts (-> (Listof HasType<%>)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define part%
  (class* document% (HasType<%> HasModel<%> HasPrice<%>)
    (super-new)

    (: get-type (-> (Option String)))
    (define/public (get-type)
      (match (get 'type)
        [(? string? s) s]
        [_ #f]))

    (: get-model (-> (Option String)))
    (define/public (get-model)
      (match (get 'model)
        [(? string? s) s]
        [_ #f]))

    (: get-price (-> (Option Number)))
    (define/public (get-price)
      (match (get 'price)
        [(? number? n) n]
        [_ #f]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Car class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define car%
  (class* document% (HasModel<%> HasPrice<%> HasParts<%>)
    (super-new)

    (: get-model (-> (Option String)))
    (define/public (get-model)
      (match (get 'model)
        [(? string? s) s]
        [_ #f]))

    (: get-price (-> (Option Number)))
    (define/public (get-price)
      (match (get 'price)
        [(? number? n) n]
        [_ #f]))

    (: get-parts (-> (Listof HasType<%>)))
    (define/public (get-parts)
      (send this children 'parts
            (λ ([p : DocProps]) (new part% [init-props p]))))))

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

(define my-car (new car% [init-props car-props]))

(printf "model: ~a\n" (send my-car get-model))
(printf "price: ~a\n" (send my-car get-price))
(printf "parts:\n")

(for-each
 (λ ([p : HasType<%>])
   (printf "\t~a\n" (send p get-type)))
 (send my-car get-parts))

(send my-car put! 'price 12000)
(printf "after price change -> price: ~a\n" (send my-car get-price))
