#lang typed/racket/base
#|
;; Triangulate 3D closed loops whose projections onto a best-fit plane are simple
;; (i.e. nonintersecting) polygons

(require (for-syntax racket/base)
         racket/match
         racket/list
         "flpoly3.rkt"
         "../math/flv3.rkt"
         "../utils.rkt")

(provide flpoly3-tessellate
         flpoly3-regularity-cutoff
         flpoly3-planarity-cutoff
         )

;; ===================================================================================================
;; Ear clipping

(: flpoly3-take (-> FlTriangle3 Fixnum Fixnum flpoly3))
(define (flpoly3-take p i1 i2)
  (match-define (flpoly3 data vs cs plane lazy-vns) p)
  (define vs1 (cyclic-subvector vs i1 (+ i2 1)))
  (define cs1 (cyclic-subvector cs i1 (+ i2 1)))
  (define n (vector-length vs1))
  (: new-vns (Lazy-Box Normals))
  (define new-vns
    (if-box-lazy?
     lazy-vns
     (λ () (lazy-box Normals))
     (λ (vns)
       (cond
         [(<= n 3)  (lazy-box Normals #())]
         [else  (define vns1 (cyclic-subvector vns i1 (+ i2 1)))
                (vector-set! vns1 0 (lazy-box (U #f FlVector)))
                (vector-set! vns1 (- n 1) (lazy-box (U #f FlVector)))
                (lazy-box Normals vns1)]))))
  (flpoly3 data vs1 cs1 plane new-vns))

(: flpoly3-clip-ear (-> flpoly3 Integer (U (List flpoly3)
                                           (List flpoly3 flpoly3))))
;; Clip a triangle off starting at index i1, if possible
(define (flpoly3-clip-ear p i1)
  (define norm (flpoly3-normal p))
  (cond
    [norm
     (define vs (flpoly3-vertices p))
     (define n (vector-length vs))
     (let* ([i1  (modulo i1 n)]
            [i2  (modulo (+ i1 1) n)])
       (define norm1 (flpoly3-vertex-normal p i2))
       (cond
         ;; Must have a normal, which must point in the same direction as p's normal
         [(or (not norm1) (<= (flv3dot norm1 norm) 0.0))  (list p)]
         [else
          (define v1 (vector-ref vs i1))
          (define v2 (vector-ref vs i2))
          (define v3 (vector-ref vs (modulo (+ i1 2) n)))
          ;; Make sure the other polygon's points aren't in it
          (let loop ([i : Nonnegative-Fixnum  (modulo (+ i1 3) n)])
            (cond [(= i i1)
                   (list (flpoly3 (flpoly3-data p)
                                  (vector v1 v2 v3)
                                  (cyclic-subvector (flpoly3-corners p) i1 (+ i1 3))
                                  (flpoly3-plane p)
                                  (lazy-box Normals #()))
                         (flpoly3-take p (+ i1 2) i1))]
                  [else
                   (define v (vector-ref vs i))
                   (cond [(triangle-point-inside? v1 v2 v3 v)  (list p)]
                         [else  (loop (modulo (+ i 1) n))])]))]))]
    [else
     (list p)]))

;; ===================================================================================================
;; Tessellating

(define flpoly3-regularity-cutoff 0.5)
(define flpoly3-planarity-cutoff (- 1.0 1e-14))

(: flpoly3-clip-first-ear (-> FlPoly3 (U (List FlPoly3)
                                         (List FlPoly3 FlPoly3))))
;; Divide at the first diagonal for which it's possible
(define (flpoly3-clip-first-ear p)
  (define n (flpoly3-length p))
  (define m (if (<= n 4) 2 n))
  (let loop ([i1 : Nonnegative-Fixnum  0])
    (cond [(< i1 m)  (define ps (flpoly3-clip-ear p i1))
                     (if (empty? (rest ps))
                         (loop (+ i1 1))
                         ps)]
          [else  (list p)])))

(: flpoly3-clip-good-ear (-> FlPoly3 (U (List FlPoly3)
                                        (List FlPoly3 FlPoly3))))
;; Clip an ear that maximizes the minimum regularity on each side of the cut
(define (flpoly3-clip-good-ear p)
  (define vs (flpoly3-vertices p))
  (define n (vector-length vs))
  (define m (if (= n 4) 2 n))
  (let loop ([i1 : Nonnegative-Fixnum  0]
             [best-ps : (U (List FlPoly3) (List FlPoly3 FlPoly3))  (list p)]
             [best-s  : Flonum  -inf.0])
    (cond [(< i1 m)
           (define ps (flpoly3-clip-ear p i1))
           (cond
             [(empty? (rest ps))  (loop (+ i1 1) best-ps best-s)]
             [else
              (define s1 (flpoly3-regularity (first ps)))
              (cond [(< s1 best-s)  (loop (+ i1 1) best-ps best-s)]
                    [else
                     (define s2 (flpoly3-regularity (second ps)))
                     (cond [(< s2 best-s)  (loop (+ i1 1) best-ps best-s)]
                           [else
                            (define s (min s1 s2))
                            (cond [(and (> n 4) (>= s flpoly3-regularity-cutoff))  ps]
                                  [else  (loop (+ i1 1) ps s)])])])])]
          [else  best-ps])))

(: flpoly3-divide (-> FlPoly3 (U (List FlPoly3)
                                 (List FlPoly3 FlPoly3))))
(define (flpoly3-divide p)
  (define n (flpoly3-length p))
  (cond [(<= n 3)
         (list p)]
        [(and (>= (flpoly3-planarity p) flpoly3-planarity-cutoff)
              (flpoly3-convex? p))
         (list p)]
        [(<= n 12)
         (flpoly3-clip-good-ear p)]
        [else
         (flpoly3-clip-first-ear p)]))

(: flpoly3-tessellate (-> FlPoly3 (Listof+1 FlPoly3)))
(define (flpoly3-tessellate p)
  (define ps (flpoly3-divide p))
  (cond [(empty? (rest ps))  ps]
        [else  (assert (append* (map flpoly3-tessellate ps)) pair?)]))

#|
(define (flpoly3-shared-edge-indexes vs1 vs2)
  (let/ec return
    (let ([vs1  (list->vector vs1)]
          [vs2  (list->vector vs2)])
      (define n1 (vector-length vs1))
      (define n2 (vector-length vs2))
      (for* ([i1  (in-range n1)]
             [i2  (in-range n2)])
        (define va1 (vector-ref vs1 i1))
        (define va2 (vector-ref vs2 i2))
        (when (<= (flv3-mag (flv3- va1 va2)) 1e-14)
          (for ([j  (in-range 1 (min n1 n2))])
            (define vb1 (vector-ref vs1 (modulo (+ i1 j) n1)))
            (define vb2 (vector-ref vs2 (modulo (- i2 j) n2)))
            (unless (<= (flv3-mag (flv3- vb1 vb2)) 1e-14)
              (for ([k  (in-range 1 (min n1 n2))])
                (define vc1 (vector-ref vs1 (modulo (- i1 k) n1)))
                (define vc2 (vector-ref vs2 (modulo (+ i2 k) n2)))
                (unless (<= (flv3-mag (flv3- vc1 vc2)) 1e-14)
                  (return (- i1 (- k 1)) (+ i1 (- j 1)) (- i2 (- j 1)) (+ i2 (- k 1))))))))))
    (values #f #f #f #f)))

(define (flpoly3-stitch vs1 ls1 vs2 ls2 i1 j1 i2 j2)
  (#;values
   canonical-polygon3d
   (append (cyclic-sublist vs1 j1 i1 #f)
           (cyclic-sublist vs2 j2 i2 #t))
   (cyclic-sublist (append (cyclic-sublist ls1 (+ j1 1) (+ i1 1) #f)
                           (cyclic-sublist ls2 (+ j2 1) (+ i2 1) #t))
                   -1 -1)))

(define (stitch-to-polygon vs1 ls1 vss lss)
  (let-values ([(vs1 ls1)  (#;values canonical-polygon3d vs1 ls1)])
    (let loop ([vs1 vs1] [ls1 ls1] [vss vss] [lss lss] [new-vss empty] [new-lss empty])
      (cond [(empty? vss)  (values vs1 ls1 new-vss new-lss)]
            [else
             (let-values ([(vs2 ls2)  (#;values canonical-polygon3d (first vss) (first lss))])
               (define-values (i1 j1 i2 j2) (flpoly3-shared-edge-indexes vs1 vs2))
               (cond [(and i1 j1 i2 j2)
                      (let-values ([(new-vs1 new-ls1)  (flpoly3-stitch vs1 ls1 vs2 ls2 i1 j1 i2 j2)])
                        (loop new-vs1 new-ls1 (rest vss) (rest lss) new -vss new-lss))]
                     [else
                      (loop vs1 ls1 (rest vss) (rest lss)
                            (cons vs2 new-vss) (cons ls2 new-lss))]))]))))

(define (stitch-polygons vss lss)
  ;(printf "vss = ~v~n" vss)
  ;(printf "lss = ~v~n~n" lss)
  (define-values (new-vss new-lss)
    (let loop ([vss vss] [lss lss])
      (cond [(empty? vss)  (values empty empty)]
            [else
             (define vs1 (first vss))
             (define ls1 (first lss))
             (let*-values ([(vs1 ls1 vss lss)  (stitch-to-polygon vs1 ls1 (rest vss) (rest lss))]
                           [(vss lss)  (loop vss lss)])
               (values (cons vs1 vss) (cons ls1 lss)))])))
  (values new-vss new-lss))
|#
|#
