#lang racket

(require file/convertible
         racket/serialize
         racket/draw)

(provide (all-defined-out))

(struct serializable-bitmap (bitmap kind)
  #:property prop:convertible
  (lambda (v req default)
    (convert (serializable-bitmap-bitmap v) req default))
  #:property prop:serializable
  (make-serialize-info
   (lambda (v)
     (define kind (serializable-bitmap-kind v))
     (case kind
       [(png)  (vector 'png (convert (serializable-bitmap-bitmap v) 'png-bytes))]
       [(jpeg)
        (define bm (serializable-bitmap-bitmap v))
        (define port (open-output-bytes))
        (send bm save-file port 'jpeg 90)
        (vector kind (get-output-bytes port))]))
   #'serializable-bitmap-deserialize-info
   #f
   (or (current-load-relative-directory)
       (current-directory))))

(define (deserialize-bitmap kind bs)
  (read-bitmap (open-input-bytes bs) kind))

(define serializable-bitmap-deserialize-info
  (make-deserialize-info deserialize-bitmap
                         (lambda () (error "no cycles"))))

(module+ deserialize-info
  (provide serializable-bitmap-deserialize-info))
