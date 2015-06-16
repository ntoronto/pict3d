#lang typed/racket/base

(require racket/list
         racket/match
         racket/unsafe/ops
         racket/flonum
         (prefix-in h: pfds/heap/pairing)
         "../math.rkt"
         "query.rkt"
         "types.rkt")

(provide face-soup-group
         face-soup-chunk)

(: face-soup-group (All (A B) (-> (face-soup A B) (Listof (Listof (face A B))))))
;; Break face soup into connected components
(define (face-soup-group fsoup)
  (define fs (face-soup-faces fsoup))
  (define l (face-soup-size fsoup))
  (cond
    [(= l 0)  empty]
    [else
     (define visited ((inst make-hasheq (face A B) #t)))
     
     (: next-dfs-state (-> (Listof (face A B)) (Listof (face A B))
                           (Values (Listof (face A B)) (Listof (face A B)))))
     (define (next-dfs-state fs stack)
       (cond
         [(empty? stack)  (values fs stack)]
         [else
          (let-values ([(f stack)  (values (first stack) (rest stack))])
            (cond
              [(hash-ref visited f #f)  (next-dfs-state fs stack)]
              [else
               (hash-set! visited f #t)
               (define-values (f12s f23s f31s) (face-soup-adjacent-faces fsoup f))
               (define inserted ((inst make-hasheq (face A B) #t)))
               (define new-stack
                 (for*/fold ([stack : (Listof (face A B))  stack])
                            ([other-fs  (in-list (list f12s f23s f31s))]
                             [other-f   (in-list other-fs)]
                             #:unless (or (hash-ref inserted other-f #f)
                                          (hash-ref visited other-f #f)))
                   (hash-set! inserted other-f #t)
                   (cons other-f stack)))
               (values (cons f fs) new-stack)]))]))
     
     (: new-fs (Listof (face A B)))
     (define new-fs
       (let loop ([fs : (Listof (face A B))  empty]
                  [stack : (Listof (face A B))  (list (first fs))])
         (define-values (new-fs new-stack) (next-dfs-state fs stack))
         (if (empty? new-stack) new-fs (loop new-fs new-stack))))
     
     (define rest-fs
       (for/fold ([rest-fs : (Listof (face A B))  empty])
                 ([f  (in-list fs)] #:unless (hash-ref visited f #f))
         (cons f rest-fs)))
     
     (if (empty? rest-fs)
         (list new-fs)
         (cons new-fs (face-soup-group (face-subsoup fsoup rest-fs))))]))

(: faces-bounds (All (A B) (-> (Listof (face A B))
                               (Values Flonum Flonum Flonum Flonum Flonum Flonum))))
(define (faces-bounds fs)
  (for/fold ([xmin : Flonum  +inf.0]
             [xmax : Flonum  -inf.0]
             [ymin : Flonum  +inf.0]
             [ymax : Flonum  -inf.0]
             [zmin : Flonum  +inf.0]
             [zmax : Flonum  -inf.0])
            ([f  (in-list fs)])
    (define v1 (vtx-position (face-vtx1 f)))
    (define v2 (vtx-position (face-vtx2 f)))
    (define v3 (vtx-position (face-vtx3 f)))
    (call/flv3-values v1
      (λ (x1 y1 z1)
        (call/flv3-values v2
          (λ (x2 y2 z2)
            (call/flv3-values v3
              (λ (x3 y3 z3)
                (values (min xmin x1 x2 x3)
                        (max xmax x1 x2 x3)
                        (min ymin y1 y2 y3)
                        (max ymax y1 y2 y3)
                        (min zmin z1 z2 z3)
                        (max zmax z1 z2 z3))))))))))

(struct axis-stats ([size : Flonum] [index : (U 0 1 2)])
  #:transparent)

(: remove-visited (All (A B) (-> (HashTable (face A B) #t) (Listof (face A B))
                                 (HashTable (face A B) #t))))
(define (remove-visited h fs)
  (for/fold ([h : (HashTable (face A B) #t)  h]) ([f  (in-list fs)])
    (hash-remove h f)))

(: face-soup-chunk (All (A B) (->* [(face-soup A B) Positive-Index] 
                                   [(-> vtx vtx Positive-Flonum Boolean)]
                                   (Listof (Listof (face A B))))))
(define (face-soup-chunk fsoup n [vtx-similar? vtx-similar?])
  (define fs (face-soup-faces fsoup))
  
  (define mean-coords ((inst make-hasheq (face A B) FlVector)))
  
  (: mean-coord (-> (face A B) (U 0 1 2) Flonum))
  (define (mean-coord f i)
    (define v (hash-ref! mean-coords f (λ () (make-flvector 3))))
    (define x (unsafe-flvector-ref v i))
    (cond [(= x 0.0)
           (define-values (v1 v2 v3) (face-flv3s f))
           (define x (* #i1/3 (+ (flv3-ref v1 i) (flv3-ref v2 i) (flv3-ref v3 i))))
           (unsafe-flvector-set! v i x)
           x]
          [else  x]))
  
  (let loop ([fs fs] [visited : (HashTable (face A B) #t)  (make-immutable-hasheq)])
    (define l (length fs))
    (cond
      [(= l 0)  empty]
      [(<= l n)  (list fs)]
      [else
       (: chunk (-> axis-stats (-> (Listof (Listof (face A B)))) (Listof (Listof (face A B)))))
       (define (chunk stats fail)
         (define i (axis-stats-index stats))
         
         ;; Best-first search for splitting and connected components
         
         (: next-bfs-state (-> (Listof (face A B))
                               (h:Heap (face A B))
                               (HashTable (face A B) #t)
                               (Values (Listof (face A B))
                                       (h:Heap (face A B))
                                       (HashTable (face A B) #t))))
         (define (next-bfs-state fs heap visited)
           (cond
             [(h:empty? heap)  (values fs heap visited)]
             [else
              (let-values ([(f heap)  (values (h:find-min/max heap) (h:delete-min/max heap))])
                (cond
                  [(hash-ref visited f #f)  (next-bfs-state fs heap visited)]
                  [else
                   (let ([fs  (cons f fs)]
                         [visited  (hash-set visited f #t)])
                     (define-values (f12s f23s f31s) (face-soup-adjacent-faces fsoup f))
                     (define-values (new-heap _)
                       (for*/fold ([heap : (h:Heap (face A B))  heap]
                                   [inserted : (Listof (face A B))  empty])
                                  ([other-fs  (in-list (list f12s f23s f31s))]
                                   [other-f   (in-list other-fs)]
                                   #:unless (or (memq other-f inserted)
                                                (hash-ref visited other-f #f)))
                         (values (h:insert other-f heap)
                                 (cons other-f inserted))))
                     (values fs new-heap visited))]))]))
         
         (: min-less? (-> (face A B) (face A B) Boolean))
         (define (min-less? f1 f2)
           (< (mean-coord f1 i) (mean-coord f2 i)))
         
         (: max-less? (-> (face A B) (face A B) Boolean))
         (define (max-less? f1 f2)
           (> (mean-coord f1 i) (mean-coord f2 i)))
         
         (define init-min-f (argmin (λ ([f : (face A B)]) (mean-coord f i)) fs))
         (define init-max-f (argmax (λ ([f : (face A B)]) (mean-coord f i)) fs))
         
         (: min-fs (Listof (face A B)))
         (: max-fs (Listof (face A B)))
         (: new-visited (HashTable (face A B) #t))
         (define-values (min-fs max-fs new-visited)
           (let loop ([min-fs : (Listof (face A B))  empty]
                      [max-fs : (Listof (face A B))  empty]
                      [visited : (HashTable (face A B) #t)  visited]
                      [min-heap : (h:Heap (face A B))  (h:heap min-less? init-min-f)]
                      [max-heap : (h:Heap (face A B))  (h:heap max-less? init-max-f)])
             (let*-values ([(min-fs min-heap visited)  (next-bfs-state min-fs min-heap visited)]
                           [(max-fs max-heap visited)  (next-bfs-state max-fs max-heap visited)])
               (if (and (h:empty? min-heap) (h:empty? max-heap))
                   (values min-fs max-fs visited)
                   (loop min-fs max-fs visited min-heap max-heap)))))
         
         (define mid-fs
           (for/fold ([mid-fs : (Listof (face A B))  empty])
                     ([f  (in-list fs)] #:unless (hash-ref new-visited f #f))
             (cons f mid-fs)))
         
         (define empty-count
           (+ (if (empty? min-fs) 1 0)
              (if (empty? mid-fs) 1 0)
              (if (empty? max-fs) 1 0)))
         
         (cond
           [(> empty-count 1)  (fail)]
           [else
            (define min-fss (loop min-fs (remove-visited new-visited min-fs)))
            (define mid-fss (loop mid-fs (remove-visited new-visited mid-fs)))
            (define max-fss (loop max-fs (remove-visited new-visited max-fs)))
            (append min-fss mid-fss max-fss)]))
     
     (define-values (xmin xmax ymin ymax zmin zmax) (faces-bounds fs))
     
     (define xsize (- xmax xmin))
     (define ysize (- ymax ymin))
     (define zsize (- zmax zmin))
     
     ;; Sort axes by size, decreasing
     (match-define (list p1 p2 p3)
       ((inst sort axis-stats Flonum)
        (list (axis-stats xsize 0)
              (axis-stats ysize 1)
              (axis-stats zsize 2))
        unsafe-fl>
        #:key axis-stats-size))
     
     ;; Try each axis in descending order of size
     (chunk p1 (λ () (chunk p2 (λ () (chunk p3 (λ () (list fs)))))))])))
