#lang typed/racket/base

(require racket/match
         racket/list
         racket/bool
         math/flonum
         math/base
         "../math.rkt"
         "../utils.rkt"
         "types.rkt"
         "query.rkt")

(provide subdivide/deform)

(define acute-threshold/1 (degrees->radians 136))
(define acute-threshold/2 (degrees->radians 121))

(: split-edges/adapt (-> (Listof (face deform-data Boolean)) FlSmooth3 Flonum Flonum
                         (edge-soup (Pair Boolean FlV3))))
(define (split-edges/adapt fs t max-edge max-angle)
  ;; Memoize the position transformation
  (define deform (make-fls3apply/pos t))
  (define deform-norm (make-fls3apply/norm t))
  
  ;; Find the allowable sector error for the given angle
  ;(define max-error (ellipse-error/angle max-angle))
  
  (: arc-or-blend-split? (-> Blend FlV3 FlV3 FlV3 FlV3 FlV3 FlV3 Boolean))
  (define (arc-or-blend-split? blend v1 vb v2 w1 wb w2)
    (define va (blend v1 vb 0.5))
    (define vc (blend vb v2 0.5))
    (define wa (fls3apply/pos t va))
    (define wc (fls3apply/pos t vc))
    (define angle
      (+ (acos (flv3bend-cos w1 wa wb))
         (acos (flv3bend-cos wa wb wc))
         (acos (flv3bend-cos wb wc w2))))
    (> (* angle (- 1.0 1e-8)) max-angle))
  
  (: maybe-add-edge (-> (Listof (edge (Pair Boolean FlV3)))
                        Blend vtx vtx FlV3 FlV3 FlV3 Boolean Boolean Flonum
                        (Listof (edge (Pair Boolean FlV3)))))
  (define (maybe-add-edge es blend vtx1 vtx2 v1 v2 v12 d12? e12? d12)
    (cond [d12?
           (define num (exact-ceiling (/ d12 max-edge)))
           (cond [(> num 1)  (define α (/ (fl (quotient num 2)) (fl num)))
                             (cons (edge vtx1 vtx2 (cons #f (blend v1 v2 α))) es)]
                 [else  (cons (edge vtx1 vtx2 (cons #f v12)) es)])]
          [e12?
           (cons (edge vtx1 vtx2 (cons #f v12)) es)]
          [else  es]))
  
  (make-edge-soup
   (for/fold ([es : (Listof (edge (Pair Boolean FlV3)))  empty]) ([f  (in-list fs)])
     (match-define (face vtx1 vtx2 vtx3 (deform-data blend _) keep12? keep23? keep31?) f)
     
     (cond
       [(and keep12? keep23? keep31?)  es]
       [else
        (define v1 (vtx-position vtx1))
        (define v2 (vtx-position vtx2))
        (define v3 (vtx-position vtx3))
        (define n1 (vtx-normal vtx1))
        (define n2 (vtx-normal vtx2))
        (define n3 (vtx-normal vtx3))
        (define w1 (deform v1))
        (define w2 (deform v2))
        (define w3 (deform v3))
        (define m1 (deform-norm v1 n1))
        (define m2 (deform-norm v2 n2))
        (define m3 (deform-norm v3 n3))
        
        (define v12 (blend v1 v2 0.5))
        (define v23 (blend v2 v3 0.5))
        (define v31 (blend v3 v1 0.5))
        
        (define d12 (flv3dist w1 w2))
        (define d23 (flv3dist w2 w3))
        (define d31 (flv3dist w3 w1))
        
        (define d12? (and (not keep12?) (> (* d12 (- 1.0 1e-8)) max-edge)))
        (define d23? (and (not keep23?) (> (* d23 (- 1.0 1e-8)) max-edge)))
        (define d31? (and (not keep31?) (> (* d31 (- 1.0 1e-8)) max-edge)))
        
        (define e12?
          (and (not keep12?)
               (or d12?
                   (and m1 m2 (> (acos (flv3cos m1 m2)) max-angle))
                   (arc-or-blend-split? blend v1 v12 v2 w1 (fls3apply/pos t v12) w2))))
        
        (define e23?
          (and (not keep23?)
               (or d23?
                   (and m2 m3 (> (acos (flv3cos m2 m3)) max-angle))
                   (arc-or-blend-split? blend v2 v23 v3 w2 (fls3apply/pos t v23) w3))))
        
        (define e31?
          (and (not keep31?)
               (or d31?
                   (and m3 m1 (> (acos (flv3cos m3 m1)) max-angle))
                   (arc-or-blend-split? blend v3 v31 v1 w3 (fls3apply/pos t v31) w1))))
        
        (let* ([es  (maybe-add-edge es blend vtx1 vtx2 v1 v2 v12 d12? e12? d12)]
               [es  (maybe-add-edge es blend vtx2 vtx3 v2 v3 v23 d23? e23? d23)]
               [es  (maybe-add-edge es blend vtx3 vtx1 v3 v1 v31 d31? e31? d31)])
          es)]))))

(: split-edges/zero-dets (-> (Listof (face deform-data Boolean)) FlSmooth3
                             (edge-soup (Pair Boolean FlV3))))
(define (split-edges/zero-dets fs t)
  ;; Memoize Jacobian determinants
  (define determinant
    (let ([memo  ((inst make-hasheq FlV3 Flonum))])
      (λ ([v : FlV3])
        (hash-ref! memo v (λ () (fls3determinant t v))))))
  
  (: find-alpha/zero-det (-> Blend FlV3 FlV3 Flonum))
  ;; Finds a blend α near a zero Jacobian determinant
  (define (find-alpha/zero-det blend v1 v2)
    (let ([α  (flbracketed-root (λ (α) (fls3determinant t (blend v1 v2 α)))
                                -1e-8
                                (+ 1.0 1e-8))])
      (if (< 0.0 α 1.0)
          (flclamp α #i1/16 (- 1.0 #i1/16))
          0.5)))
  
  (: maybe-add-edge (-> (Listof (edge (Pair Boolean FlV3))) vtx vtx Flonum Flonum Blend
                        (Listof (edge (Pair Boolean FlV3)))))
  (define (maybe-add-edge es vtx1 vtx2 d1 d2 blend)
    ;; Determine whether the edge passes through a discontinuity
    (define m (max (abs d1) (abs d2)))
    (cond [(or (and (< -inf.0 d1 (* m -1e-8)) (< (* m +1e-8) d2 +inf.0))
               (and (< -inf.0 d2 (* m -1e-8)) (< (* m +1e-8) d1 +inf.0)))
           (define v1 (vtx-position vtx1))
           (define v2 (vtx-position vtx2))
           ;; We get nice results by splitting them as close as possible to the discontinuity
           (define α (find-alpha/zero-det blend v1 v2))
           (define v12 (blend v1 v2 α))
           (cons (edge vtx1 vtx2 (cons #f v12)) es)]
          [else  es]))
  
  (make-edge-soup
   (for/fold ([es : (Listof (edge (Pair Boolean FlV3)))  empty]) ([f  (in-list fs)])
     (define-values (vtx1 vtx2 vtx3) (face-vtxs f))
     (define blend (deform-data-blend (face-data f)))
     
     ;; Compute Jacobian determinants at each vertex position
     (define d1 (determinant (vtx-position vtx1)))
     (define d2 (determinant (vtx-position vtx2)))
     (define d3 (determinant (vtx-position vtx3)))
     
     (let* ([es  (maybe-add-edge es vtx1 vtx2 d1 d2 blend)]
            [es  (maybe-add-edge es vtx2 vtx3 d2 d3 blend)]
            [es  (maybe-add-edge es vtx3 vtx1 d3 d1 blend)])
       es))))

(: face-edge-vertices (All (A B) (-> (face A B) FlV3 FlV3 (Values vtx vtx))))
(define (face-edge-vertices f va vb)
  (define-values (vtx1 vtx2 vtx3) (face-vtxs f))
  (define v1 (vtx-position vtx1))
  (define v2 (vtx-position vtx2))
  (define v3 (vtx-position vtx3))
  (cond [(or (and (flv3=? v1 va) (flv3=? v2 vb))
             (and (flv3=? v1 vb) (flv3=? v2 va)))
         (values vtx1 vtx2)]
        [(or (and (flv3=? v2 va) (flv3=? v3 vb))
             (and (flv3=? v2 vb) (flv3=? v3 va)))
         (values vtx2 vtx3)]
        [(or (and (flv3=? v3 va) (flv3=? v1 vb))
             (and (flv3=? v3 vb) (flv3=? v1 va)))
         (values vtx3 vtx1)]
        [else
         (raise-argument-error 'face-edge-vertices
                               (format "face with vertices at ~v and ~v" va vb)
                               f)]))

(: new-edge-vertex (-> (face deform-data Boolean)
                       (Listof+1 (edge (Pair Boolean FlV3)))
                       (Values Boolean vtx)))
(define (new-edge-vertex f es)
  (define interp (deform-data-interp (face-data f)))
  (define e (first es))
  (define v1 (vtx-position (edge-vtx1 e)))
  (define v2 (vtx-position (edge-vtx2 e)))
  (define-values (vtx1 vtx2) (face-edge-vertices f v1 v2))
  (define new-vtx
    (interp vtx1 vtx2 (flv3mean* (map (λ ([e : (edge (Pair Boolean FlV3))])
                                        (cdr (edge-data e)))
                                      es))))
  (define admin?
    (andmap (λ ([e : (edge (Pair Boolean FlV3))])
              (car (edge-data e)))
            es))
  (values admin? new-vtx))

(: fix-edges (All (A) (-> (Listof (face deform-data Boolean))
                          (edge-soup (Pair Boolean FlV3))
                          FlSmooth3
                          (edge-soup (Pair Boolean FlV3)))))
(define (fix-edges fs edges t)
  ;; Memoize the position transformation
  (define deform (make-fls3apply/pos t))
  
  ;; Doesn't seem to help
  (: maybe-fix/1 (-> (Listof (edge (Pair Boolean FlV3))) Blend vtx vtx vtx
                     (Listof (edge (Pair Boolean FlV3)))))
  ;; Splitting on v12
  (define (maybe-fix/1 es blend vtx1 vtx2 vtx3)
    (define v1 (vtx-position vtx1))
    (define v2 (vtx-position vtx2))
    (define v3 (vtx-position vtx3))
    (define w1 (deform v1))
    (define w2 (deform v2))
    (define w3 (deform v3))
    (define angle (acos (flv3bend-cos w2 w3 w1)))
    (if (> angle acute-threshold/2)
        (list* (edge vtx2 vtx3 (cons #t (blend v2 v3 0.5)))
               (edge vtx3 vtx1 (cons #t (blend v3 v1 0.5)))
               es)
        es))
  
  (: maybe-fix/2 (-> (Listof (edge (Pair Boolean FlV3))) Blend vtx vtx vtx
                     (Listof (edge (Pair Boolean FlV3)))))
  ;; Splitting on v23 and v31
  (define (maybe-fix/2 es blend vtx1 vtx2 vtx3)
    (define v1 (vtx-position vtx1))
    (define v2 (vtx-position vtx2))
    (define v3 (vtx-position vtx3))
    (define w1 (deform v1))
    (define w2 (deform v2))
    (define w3 (deform v3))
    (define v23 (blend v2 v3 0.5))
    (define v31 (blend v3 v1 0.5))
    (define w23 (fls3apply/pos t v23))
    (define w31 (fls3apply/pos t v31))
    (define angle (if (< (flv3dist w23 w1)
                         (flv3dist w2 w31))
                      (max (acos (flv3bend-cos w31 w1 w2))
                           (acos (flv3bend-cos w2 w23 w31)))
                      (max (acos (flv3bend-cos w1 w2 w23))
                           (acos (flv3bend-cos w23 w31 w1)))))
    (if (> angle acute-threshold/2)
        (cons (edge vtx1 vtx2 (cons #t (blend v1 v2 0.5))) es)
        es))
  
  (let loop ([edges : (edge-soup (Pair Boolean FlV3))  edges])
    (define new-es
      (for/fold ([es : (Listof (edge (Pair Boolean FlV3)))  empty]) ([f  (in-list fs)])
        (define-values (e12s e23s e31s) (edge-soup-face-edges edges f))
        (define-values (vtx1 vtx2 vtx3) (face-vtxs f))
        (define blend (deform-data-blend (face-data f)))
        (define num-split (+ (if (empty? e12s) 0 1) (if (empty? e23s) 0 1) (if (empty? e31s) 0 1)))
        (case num-split
          #;; Doesn't seem to help
          [(1)  (cond [(pair? e12s)  (maybe-fix/1 es blend vtx1 vtx2 vtx3)]
                      [(pair? e23s)  (maybe-fix/1 es blend vtx2 vtx3 vtx1)]
                      [(pair? e31s)  (maybe-fix/1 es blend vtx3 vtx1 vtx2)]
                      [else  (error 'impossible)])]
          [(2)  (cond [(and (pair? e12s) (pair? e23s))  (maybe-fix/2 es blend vtx3 vtx1 vtx2)]
                      [(and (pair? e23s) (pair? e31s))  (maybe-fix/2 es blend vtx1 vtx2 vtx3)]
                      [(and (pair? e31s) (pair? e12s))  (maybe-fix/2 es blend vtx2 vtx3 vtx1)]
                      [else  (error 'impossible)])]
          [else  es])))
    (if (empty? new-es)
        edges
        (loop (make-edge-soup (append new-es (edge-soup-edges edges)))))))

(: subdivide-faces (-> (Listof (face deform-data Boolean))
                       (edge-soup (Pair Boolean FlV3))
                       FlSmooth3
                       (Values (Listof (face deform-data Boolean))
                               (Listof (face deform-data Boolean)))))
(define (subdivide-faces fs edges t)
  ;; Memoize the position transformation
  (define deform (make-fls3apply/pos t))
  
  (: subdivide/1 (-> vtx vtx vtx Boolean Boolean Boolean vtx Boolean deform-data
                     (Listof (face deform-data Boolean))))
  (define (subdivide/1 v1 v2 v3 k12? k23? k31? v12 a12? data)
    (list (face v1 v12 v3 data (or k12? a12?) a12? k31?)
          (face v12 v2 v3 data (or k12? a12?) k23? a12?)))
  
  (: subdivide/2 (-> vtx vtx vtx Boolean Boolean Boolean
                     vtx vtx Boolean Boolean
                     deform-data
                     (Listof (face deform-data Boolean))))
  (define (subdivide/2 v1 v2 v3 k12? k23? k31? v12 v23 a12? a23? data)
    (define w1 (deform (vtx-position v1)))
    (define w3 (deform (vtx-position v3)))
    (define w12 (deform (vtx-position v12)))
    (define w23 (deform (vtx-position v23)))
    (cons (face v2 v23 v12 data (or k23? a23?) (and a23? a12?) (or k12? a12?))
          (if (< (flv3dist w12 w3)
                 (flv3dist w1 w23))
              (list (face v3 v12 v23 data a12? (and a12? a23?) (or k23? a23?))
                    (face v1 v12 v3 data (or k12? a12?) a12? k31?))
              (list (face v1 v12 v23 data (or k12? a12?) (and a12? a23?) a23?)
                    (face v1 v23 v3 data a23? (and k23? a23?) k31?)))))
  
  (: subdivide/3 (-> vtx vtx vtx Boolean Boolean Boolean
                     vtx vtx vtx Boolean Boolean Boolean
                     deform-data
                     (Listof (face deform-data Boolean))))
  (define (subdivide/3 v1 v2 v3 k12? k23? k31? v12 v23 v31 a12? a23? a31? data)
    (list (face v1 v12 v31 data (or a12? k12?) (and a12? a31?) (or a31? k31?))
          (face v2 v23 v12 data (or a23? k23?) (and a23? a12?) (or a12? k12?))
          (face v3 v31 v23 data (or a31? k31?) (and a31? a23?) (or a23? k23?))
          (face v12 v23 v31 data (and a12? a23?) (and a23? a31?) (and a31? a12?))))
  
  (define-values (new-fs old-fs)
    (for/fold ([new-fs : (Listof (face deform-data Boolean))  empty]
               [old-fs : (Listof (face deform-data Boolean))  empty])
              ([f  (in-list fs)])
      (match-define (face v1 v2 v3 data k12? k23? k31?) f)
      (define-values (e12s e23s e31s) (edge-soup-face-edges edges f))
      (define num-empty (+ (if (empty? e12s) 1 0) (if (empty? e23s) 1 0) (if (empty? e31s) 1 0)))
      (case num-empty
        [(0)  (values
               (append
                (cond [(and (pair? e12s) (pair? e23s) (pair? e31s))
                       (define-values (a12? v12) (new-edge-vertex f e12s))
                       (define-values (a23? v23) (new-edge-vertex f e23s))
                       (define-values (a31? v31) (new-edge-vertex f e31s))
                       (subdivide/3 v1 v2 v3 k12? k23? k31? v12 v23 v31 a12? a23? a31? data)]
                      [else
                       (error 'impossible)])
                new-fs)
               old-fs)]
        [(1)  (values
               (append
                (cond [(and (pair? e12s) (pair? e23s))
                       (define-values (a12? v12) (new-edge-vertex f e12s))
                       (define-values (a23? v23) (new-edge-vertex f e23s))
                       (subdivide/2 v1 v2 v3 k12? k23? k31? v12 v23 a12? a23? data)]
                      [(and (pair? e23s) (pair? e31s))
                       (define-values (a23? v23) (new-edge-vertex f e23s))
                       (define-values (a31? v31) (new-edge-vertex f e31s))
                       (subdivide/2 v2 v3 v1 k23? k31? k12? v23 v31 a23? a31? data)]
                      [(and (pair? e31s) (pair? e12s))
                       (define-values (a31? v31) (new-edge-vertex f e31s))
                       (define-values (a12? v12) (new-edge-vertex f e12s))
                       (subdivide/2 v3 v1 v2 k31? k12? k23? v31 v12 a31? a12? data)]
                      [else
                       (error 'impossible)])
                new-fs)
               old-fs)]
        [(2)  (values
               (append
                (cond [(pair? e12s)
                       (define-values (a12? v12) (new-edge-vertex f e12s))
                       (subdivide/1 v1 v2 v3 k12? k23? k31? v12 a12? data)]
                      [(pair? e23s)
                       (define-values (a23? v23) (new-edge-vertex f e23s))
                       (subdivide/1 v2 v3 v1 k23? k31? k12? v23 a23? data)]
                      [(pair? e31s)
                       (define-values (a31? v31) (new-edge-vertex f e31s))
                       (subdivide/1 v3 v1 v2 k31? k12? k23? v31 a31? data)]
                      [else  (error 'impossible)])
                new-fs)
               old-fs)]
        [else
         (values new-fs (cons f old-fs))])))
  (values new-fs old-fs))

(: subdivide/deform (-> (face-soup deform-data Boolean)
                        FlSmooth3
                        Positive-Flonum
                        Nonnegative-Flonum
                        Natural
                        (face-soup deform-data Boolean)))
(define (subdivide/deform faces0 t max-edge max-angle max-iters)
  (define-syntax-rule (subdivide faces start stop split-edges)
    (ann
     (let loop ([faces : (face-soup deform-data Boolean)  faces] [i start])
       (cond
         [(< i stop)
          (define edges (split-edges (face-soup-faces faces)))
          (cond
            [(= 0 (edge-soup-size edges))  faces]
            [else
             (define fs (face-soup-faces faces))
             (define new-edges (fix-edges fs edges t))
             (define-values (new-fs old-fs) (subdivide-faces fs new-edges t))
             (loop (make-face-soup (append new-fs old-fs)) (+ i 1))])]
         [else  faces]))
     (face-soup deform-data Boolean)))
  
  (define faces2 (subdivide faces0 0 max-iters
                            (λ (faces) (split-edges/adapt faces t max-edge max-angle))))
  (cond [(flaffine3? t)  faces2]
        [else  (subdivide faces2 0 3 (λ (faces) (split-edges/zero-dets faces t)))]))
