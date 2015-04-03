#lang typed/racket

(require pict3d)

(provide add-debug-pict!
         process-debug-picts)

(: debug-picts (HashTable Symbol (Pair Pict3D Flonum)))
(define debug-picts (make-hasheq))

(: add-debug-pict! (->* [Pict3D Flonum] [Symbol] Void))
(define (add-debug-pict! p time [name (gensym)])
  (hash-set! debug-picts name (cons p time)))

(: process-debug-picts (-> Flonum (Listof Pict3D)))
(define (process-debug-picts dtime)
  (define ks (hash-keys debug-picts))
  
  (define picts
    (for/fold ([picts : (Listof Pict3D)  empty]) ([k  (in-list ks)])
      (match-define (cons pict timeout) (hash-ref debug-picts k))
      (define new-timeout (- timeout dtime))
      (cond [(< new-timeout 0.0)  (hash-remove! debug-picts k)
                                  picts]
            [else  (hash-set! debug-picts k (cons pict new-timeout))
                   (cons pict picts)])))
  
  picts)
