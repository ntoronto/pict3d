#lang typed/racket/base

(require racket/list
         "../math.rkt"
         "types.rkt")

(provide (all-defined-out))

(: face-soup-corner-faces (All (A B) (-> (face-soup A B)
                                         FlV3
                                         (Listof (Pair Face-Index (face A B))))))
(define (face-soup-corner-faces faces v)
  (hash-ref (face-soup-flv3-face-hash faces) v (λ () empty)))

(: face-soup-adjacent-faces (All (A1 B1 A2 B2) (-> (face-soup A1 B1)
                                                   (face A2 B2)
                                                   (Values (Listof (face A1 B1))
                                                           (Listof (face A1 B1))
                                                           (Listof (face A1 B1))))))
(define (face-soup-adjacent-faces faces this-f)
  (define-values (v1 v2 v3) (face-flv3s this-f))
  (define h (face-soup-flv3-face-hash faces))
  (define f1s (hash-ref h v1 (λ () empty)))
  (define f2s (hash-ref h v2 (λ () empty)))
  (define f3s (hash-ref h v3 (λ () empty)))
  (define f12s : (Listof (face A1 B1))  empty)
  (define f23s : (Listof (face A1 B1))  empty)
  (define f31s : (Listof (face A1 B1))  empty)
  (let*-values
      ([(f31s f12s)
        (for/fold ([f31s : (Listof (face A1 B1))  f31s]
                   [f12s : (Listof (face A1 B1))  f12s])
                  ([i+f  (in-list f1s)] #:unless (eq? (cdr i+f) this-f))
          (define f (cdr i+f))
          (define-syntax-rule (v4) (vtx-position (face-vtx1 f)))
          (define-syntax-rule (v5) (vtx-position (face-vtx2 f)))
          (define-syntax-rule (v6) (vtx-position (face-vtx3 f)))
          (case (car i+f)
            ;; v1 = v4
            [(1)   (values (if (flv3=? v3 (v5)) (cons f f31s) f31s)
                           (if (flv3=? v2 (v6)) (cons f f12s) f12s))]
            ;; v1 = v5
            [(2)   (values (if (flv3=? v3 (v6)) (cons f f31s) f31s)
                           (if (flv3=? v2 (v4)) (cons f f12s) f12s))]
            ;; v1 = v6
            [else  (values (if (flv3=? v3 (v4)) (cons f f31s) f31s)
                           (if (flv3=? v2 (v5)) (cons f f12s) f12s))]))]
       [(f12s f23s)
        (for/fold ([f12s : (Listof (face A1 B1))  f12s]
                   [f23s : (Listof (face A1 B1))  f23s])
                  ([i+f  (in-list f2s)] #:unless (eq? (cdr i+f) this-f))
          (define f (cdr i+f))
          (define-syntax-rule (v4) (vtx-position (face-vtx1 f)))
          (define-syntax-rule (v5) (vtx-position (face-vtx2 f)))
          (define-syntax-rule (v6) (vtx-position (face-vtx3 f)))
          (case (car i+f)
            ;; v2 = v4
            [(1)   (values (if (flv3=? v1 (v5)) (cons f f12s) f12s)
                           (if (flv3=? v3 (v6)) (cons f f23s) f23s))]
            ;; v2 = v5
            [(2)   (values (if (flv3=? v1 (v6)) (cons f f12s) f12s)
                           (if (flv3=? v3 (v4)) (cons f f23s) f23s))]
            ;; v2 = v6
            [else  (values (if (flv3=? v1 (v4)) (cons f f12s) f12s)
                           (if (flv3=? v3 (v5)) (cons f f23s) f23s))]))]
       [(f23s f31s)
        (for/fold ([f23s : (Listof (face A1 B1))  f23s]
                   [f31s : (Listof (face A1 B1))  f31s])
                  ([i+f  (in-list f3s)] #:unless (eq? (cdr i+f) this-f))
          (define f (cdr i+f))
          (define-syntax-rule (v4) (vtx-position (face-vtx1 f)))
          (define-syntax-rule (v5) (vtx-position (face-vtx2 f)))
          (define-syntax-rule (v6) (vtx-position (face-vtx3 f)))
          (case (car i+f)
            ;; v3 = v4
            [(1)   (values (if (flv3=? v2 (v5)) (cons f f23s) f23s)
                           (if (flv3=? v1 (v6)) (cons f f31s) f31s))]
            ;; v3 = v5
            [(2)   (values (if (flv3=? v2 (v6)) (cons f f23s) f23s)
                           (if (flv3=? v1 (v4)) (cons f f31s) f31s))]
            ;; v3 = v6
            [else  (values (if (flv3=? v2 (v4)) (cons f f23s) f23s)
                           (if (flv3=? v1 (v5)) (cons f f31s) f31s))]))])
    (values (remove-duplicates f12s eq?)
            (remove-duplicates f23s eq?)
            (remove-duplicates f31s eq?))))

(: edge-soup-face-edges (All (B1 A2 B2) (-> (edge-soup B1)
                                            (face A2 B2)
                                            (Values (Listof (edge B1))
                                                    (Listof (edge B1))
                                                    (Listof (edge B1))))))
(define (edge-soup-face-edges edges f)
  (define h (edge-soup-flv3-edge-hash edges))
  (define-values (v1 v2 v3) (face-flv3s f))
  (define e1s (hash-ref h v1 (λ () empty)))
  (define e2s (hash-ref h v2 (λ () empty)))
  (define e3s (hash-ref h v3 (λ () empty)))
  (define e12s : (Listof (edge B1))  empty)
  (define e23s : (Listof (edge B1))  empty)
  (define e31s : (Listof (edge B1))  empty)
  (let*-values
      ([(e31s e12s)
        (for/fold ([e31s : (Listof (edge B1))  e31s]
                   [e12s : (Listof (edge B1))  e12s])
                  ([e  (in-list e1s)])
          (define src (vtx-position (edge-vtx1 e)))
          (define dst (vtx-position (edge-vtx2 e)))
          ;; Known: v1 = src xor v1 = dst
          (if (flv3=? v1 src)
              (values (if (flv3=? v3 dst) (cons e e31s) e31s)
                      (if (flv3=? v2 dst) (cons e e12s) e12s))
              (values (if (flv3=? v3 src) (cons e e31s) e31s)
                      (if (flv3=? v2 src) (cons e e12s) e12s))))]
       [(e12s e23s)
        (for/fold ([e12s : (Listof (edge B1))  e12s]
                   [e23s : (Listof (edge B1))  e23s])
                  ([e  (in-list e2s)])
          (define src (vtx-position (edge-vtx1 e)))
          (define dst (vtx-position (edge-vtx2 e)))
          ;; Known: v2 = src xor v2 = dst
          (if (flv3=? v2 src)
              (values (if (flv3=? v1 dst) (cons e e12s) e12s)
                      (if (flv3=? v3 dst) (cons e e23s) e23s))
              (values (if (flv3=? v1 src) (cons e e12s) e12s)
                      (if (flv3=? v3 src) (cons e e23s) e23s))))]
       [(e23s e31s)
        (for/fold ([e23s : (Listof (edge B1))  e23s]
                   [e31s : (Listof (edge B1))  e31s])
                  ([e  (in-list e3s)])
          (define src (vtx-position (edge-vtx1 e)))
          (define dst (vtx-position (edge-vtx2 e)))
          ;; Known: v3 = src xor v3 = dst
          (if (flv3=? v3 src)
              (values (if (flv3=? v2 dst) (cons e e23s) e23s)
                      (if (flv3=? v1 dst) (cons e e31s) e31s))
              (values (if (flv3=? v2 src) (cons e e23s) e23s)
                      (if (flv3=? v1 src) (cons e e31s) e31s))))])
    (values (remove-duplicates e12s eq?)
            (remove-duplicates e23s eq?)
            (remove-duplicates e31s eq?))))
