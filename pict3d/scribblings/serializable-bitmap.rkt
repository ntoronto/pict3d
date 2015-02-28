#lang racket

(require file/convertible
         racket/serialize
         racket/draw)

(provide (all-defined-out))

(struct serializable-bitmap (bitmap)
  #:property prop:convertible
  (lambda (v req default)
    (convert (serializable-bitmap-bitmap v) req default))
  #:property prop:serializable
  (make-serialize-info
   (lambda (v) (vector (convert v 'png-bytes)))
   #'serializable-bitmap-deserialize-info
   #f
   (or (current-load-relative-directory)
       (current-directory))))

(define (deserialize-bitmap bs)
  (read-bitmap (open-input-bytes bs) 'png/alpha))

(define serializable-bitmap-deserialize-info
  (make-deserialize-info deserialize-bitmap
                         (lambda () (error "no cycles"))))

(module+ deserialize-info
  (provide serializable-bitmap-deserialize-info))
