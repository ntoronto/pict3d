#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         racket/vector
         racket/flonum
         racket/unsafe/ops)

(provide (all-defined-out))

(require/typed
 profile
 [profile-thunk  (-> (-> Any) Any)])

(define-syntax-rule (profile body ...)
  (profile-thunk (λ () body ...)))

;; ===================================================================================================
;; Lists with minimum length

(define-type (Listof+1 A) (Pair A (Listof A)))
(define-type (Listof+2 A) (Pair A (Listof+1 A)))
(define-type (Listof+3 A) (Pair A (Listof+2 A)))
(define-type (Listof+4 A) (Pair A (Listof+3 A)))

(define-syntax-rule (list+1? xs)
  (not (empty? xs)))

(define-syntax-rule (list+2? xs)
  (not (or (empty? xs) (empty? (cdr xs)))))

(define-syntax-rule (list+3? xs)
  (not (or (empty? xs) (empty? (cdr xs)) (empty? (cdr (cdr xs))))))

(define-syntax-rule (list+4? xs)
  (not (or (empty? xs) (empty? (cdr xs)) (empty? (cdr (cdr xs))) (empty? (cdr (cdr (cdr xs)))))))

(: map+1 (All (A B) (-> (-> A B) (Listof+1 A) (Listof+1 B))))
(define (map+1 f xs)
  (map f xs))

(: map+2 (All (A B) (-> (-> A B) (Listof+2 A) (Listof+2 B))))
(define (map+2 f xs)
  (cons (f (car xs)) (map+1 f (cdr xs))))

(: map+3 (All (A B) (-> (-> A B) (Listof+3 A) (Listof+3 B))))
(define (map+3 f xs)
  (cons (f (car xs)) (map+2 f (cdr xs))))

(: map+4 (All (A B) (-> (-> A B) (Listof+4 A) (Listof+4 B))))
(define (map+4 f xs)
  (cons (f (car xs)) (map+3 f (cdr xs))))

;; ===================================================================================================
;; Lists as cyclic values

(: sublist (All (A) (-> (Listof A) Integer Integer (Listof A))))
(define (sublist xs i1 i2)
  (take (drop xs i1) (- i2 i1)))

(: cyclic-sublist (All (A) (->* ((Listof A) Integer Integer) ((U 'full 'empty 'error)) (Listof A))))
(define (cyclic-sublist xs i1 i2 [equal 'error])
  (define n (length xs))
  (if (= n 0)
      (if (eq? equal 'error)
          (raise-type-error 'cyclic-sublist "nonempty list" 0 xs i1 i2 equal)
          empty)
      (let ([i1  (modulo i1 n)]
            [i2  (modulo i2 n)])
        (cond [(i1 . < . i2)  (sublist xs i1 i2)]
              [(i1 . > . i2)  (append (drop xs i1) (take xs i2))]
              [else
               (case equal
                 [(full)   (append (drop xs i1) (take xs i2))]
                 [(empty)  empty]
                 [(error)  (raise-type-error 'cyclic-sublist
                                             (format "Integer, not ~a" i1)
                                             2 xs i1 i2)])]))))

(: cyclic-subvector (All (A) (->* [(Vectorof A) Integer Integer]
                                  [(U 'full 'empty 'error)]
                                  (Vectorof A))))
(define (cyclic-subvector xs i1 i2 [equal 'error])
  (define n (vector-length xs))
  (if (= n 0)
      (if (eq? equal 'error)
          (raise-type-error 'cyclic-subvector "nonempty vector" 0 xs i1 i2 equal)
          #())
      (let ([i1  (modulo i1 n)]
            [i2  (modulo i2 n)])
        (cond [(i1 . < . i2)
               (vector-copy xs i1 i2)]
              [(or (eq? equal 'full) (i1 . > . i2))
               (define m1 (- n i1))
               (define ys ((inst make-vector A) (+ i2 m1) (vector-ref xs 0)))
               (vector-copy! ys 0 xs i1 n)
               (vector-copy! ys m1 xs 0 i2)
               ys]
              [(eq? equal 'empty)
               #()]
              [else
               (raise-type-error 'cyclic-subvector
                                 (format "Integer, not ~a" i1)
                                 2 xs i1 i2)]))))

(: list-rotate-left (All (A) (-> (Listof A) Integer (Listof A))))
(define (list-rotate-left xs i)
  (cond [(empty? xs)  empty]
        [else  (let ([i  (modulo i (length xs))])
                 (append (drop xs i) (take xs i)))]))

(: list-rotate-right (All (A) (-> (Listof A) Integer (Listof A))))
(define (list-rotate-right xs i)
  (cond [(empty? xs)  empty]
        [else  (let ([i  (modulo (- i) (length xs))])
                 (append (drop xs i) (take xs i)))]))

;; ===================================================================================================
;; Indexes of polygon diagonals

(: polygon-triangle-indexes (-> Index (Listof (Pair Index Index))))
(define (polygon-triangle-indexes n)
  (cond [(< n 4)  empty]
        [(= n 4)  '((0 . 2) (1 . 3))]
        [else
         (for*/list : (Listof (Pair Index Index)) ([i1 : Integer  (in-range n)])
           (cons (assert i1 index?) (modulo (+ i1 2) n)))]))

(: polygon-diagonal-indexes (-> Index (Listof (Pair Index Index))))
(define (polygon-diagonal-indexes n)
  (for*/list : (Listof (Pair Index Index)) ([i1 : Integer  (in-range (- n 2))]
                                            [i2 : Integer  (in-range (+ i1 2)
                                                                     (+ n (min 0 (- i1 1))))])
    (cons (assert i1 index?) (modulo i2 n))))

(: polygon-opposite-indexes (-> Index (Listof (Pair Index Index))))
(define (polygon-opposite-indexes n)
  (cond [(n . < . 4)  empty]
        [else
         (define half (quotient n 2))
         (define half+1 (quotient (+ n 1) 2))
         (cond [(= half half+1)
                (for*/list : (Listof (Pair Index Index)) ([i1 : Integer  (in-range half)])
                  (cons (assert i1 index?) (modulo (+ i1 half) n)))]
               [else
                (append
                 (for*/list : (Listof (Pair Index Index)) ([i1 : Integer  (in-range half+1)])
                   (cons (assert i1 index?) (modulo (+ i1 half) n)))
                 (for*/list : (Listof (Pair Index Index)) ([i1 : Integer  (in-range half)])
                   (cons (assert i1 index?) (modulo (+ i1 half 1) n))))])]))

;; ===================================================================================================
;; Lazy boxes: like promises, but *clients* specify how to compute the values

(define-type (Lazy-Box A) (Boxof (U 'lazy A)))

(define-syntax (lazy-box stx)
  (syntax-case stx ()
    [(_ A)  (syntax/loc stx ((inst box (U 'lazy A)) 'lazy))]
    [(_ A e)  (syntax/loc stx ((inst box (U 'lazy A)) e))]))

(define-syntax (lazy-box-ref! stx)
  (syntax-case stx ()
    [(_ bx-stx thnk)
     (syntax/loc stx
       (let* ([bx  bx-stx]
              [val  (unbox bx)])
         (cond [(eq? 'lazy val)  (define val (thnk))
                                 (set-box! bx val)
                                 val]
               [else  val])))]))

;(: if-lazy-box (All (A B) (-> (Lazy-Box A) (-> B) (-> A B) B)))
(define-syntax (if-box-lazy? stx)
  (syntax-case stx ()
    [(_ bx-stx then-thnk else-proc)
     (syntax/loc stx
       (let ([bx  bx-stx])
         (define val (unbox bx))
         (cond [(eq? 'lazy val)
                (then-thnk)]
               [else
                (else-proc val)])))]))

;; ===================================================================================================
;; Hash functions

(: hash-clear!* (All (A B) (-> (HashTable A B) Void)))
(define (hash-clear!* h)
  (for ([k  (in-list (hash-keys h))])
    (hash-remove! h k)))

(define-type (List-Hash A B) (Listof (Pair A B)))

(: list-hasheq-ref (All (A B) (->* [(List-Hash A B) A] [(-> B)] B)))
(define (list-hasheq-ref orig-h k [thnk #f])
  (let loop ([h orig-h])
    (cond [(empty? h)  (if thnk (thnk) (error 'list-hasheq-ref "no key ~e in hash ~e" k orig-h))]
          [else
           (define kv (first h))
           (cond [(eq? (car kv) k)  (cdr kv)]
                 [else  (loop (rest h))])])))

(: list-hasheq-remove (All (A B) (-> (List-Hash A B) A (List-Hash A B))))
(define (list-hasheq-remove h k)
  (let loop ([h h])
    (cond [(empty? h)  empty]
          [else
           (define kv (first h))
           (cond [(eq? (car kv) k)  (rest h)]
                 [else
                  (define rest-h (rest h))
                  (define new-rest-h (loop rest-h))
                  (if (eq? new-rest-h rest-h) h (cons kv new-rest-h))])])))

(: list-hasheq-set (All (A B) (-> (List-Hash A B) A B (List-Hash A B))))
(define (list-hasheq-set h k v)
  (cons (cons k v)
        (list-hasheq-remove h k)))

(: list-hasheq-merge (All (A B) (-> (List-Hash A B) (List-Hash A B) (List-Hash A B))))
(define (list-hasheq-merge h1 h2)
  ((inst remove-duplicates (Pair A B) A) (append h1 h2) eq? #:key car))

;; ===================================================================================================
;; Vector operations

(: vector-rmap (All (A B) (-> (-> A B) (Vectorof A) (Vectorof B))))
(define (vector-rmap f vs)
  (define n (vector-length vs))
  (cond [(= n 0)  #()]
        [else
         (define new-vs (make-vector n (f (vector-ref vs 0))))
         (define n-1 (- n 1))
         (for ([i  (in-range 0 n-1)]
               [j  (in-range n-1 0 -1)])
           (vector-set! new-vs i (f (vector-ref vs j))))
         new-vs]))

(: vector-reverse (All (A) (-> (Vectorof A) (Vectorof A))))
(define (vector-reverse vs)
  (vector-rmap (λ ([a : A]) a) vs))

;; ===================================================================================================
;; FlVector operations

(define-syntax (flvector-values stx)
  (syntax-case stx ()
    [(_ v-stx n)
     (exact-nonnegative-integer? (syntax->datum #'n))
     (with-syntax ([(i ...)  (build-list (syntax->datum #'n) values)])
       (syntax/loc stx
         (let ([v : FlVector  v-stx])
           (unless (= n (flvector-length v))
             (raise-type-error 'flvector-values (format "length-~a FlVector" n) v))
           (values (unsafe-flvector-ref v i) ...))))]))
