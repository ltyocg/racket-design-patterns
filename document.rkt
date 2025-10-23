#lang racket
(module+ immutable
  ;; 不可变文档操作
  (define (document-put-immutable doc key value)
    (hash-set doc key value))

  ;; 文档转换器
  (define (document-map doc proc)
    (hash-map doc (lambda (k v) (proc k v))))

  ;; 特质组合器
  (define (with-type type doc)
    (document-put-immutable doc 'type type))

  (define (with-model model doc)
    (document-put-immutable doc 'model model))

  (define (with-price price doc)
    (document-put-immutable doc 'price price))

  (define (with-parts parts doc)
    (document-put-immutable doc 'parts parts))

  ;; 构建汽车的更函数式的方式
  (define (create-car)
    (define empty-doc (hash))
    (-> empty-doc
        (with-model "300SL")
        (with-price 10000)
        (with-parts (list (-> (hash)
                              (with-type "wheel")
                              (with-model "15C")
                              (with-price 100))
                          (-> (hash)
                              (with-type "door")
                              (with-model "Lambo")
                              (with-price 300))))))

  ;; 查询接口
  (define (select-keys doc keys)
    (for/hash ([key keys] #:when (hash-has-key? doc key))
      (values key (hash-ref doc key))))

  (define (where doc predicate)
    (if (predicate doc) doc #f)))