#lang typed/racket/base

(require "flv3.rkt"
         "flplane3.rkt"
         "flv4.rkt"
         "fllinear3.rkt"
         "flaffine3.rkt"
         "flprojective3.rkt")

(provide (all-defined-out)
         (all-from-out
          "fllinear3.rkt"
          "flaffine3.rkt"
          "flprojective3.rkt"))

;; ===================================================================================================

(define-type FlAffLin3 (U FlLinear3 FlAffine3))
(define-type FlTransform3 (U FlAffLin3 FlProjective3))

(define flafflin3? (λ (v) (or (fllinear3? v) (flaffine3? v))))
(define fltransform3? (λ (v) (or (flafflin3? v) (flprojective3? v))))

;; ===================================================================================================

(: flt3determinant (-> FlTransform3 Flonum))
(define (flt3determinant t)
  (cond [(fllinear3? t)  (fllinear3-determinant t)]
        [(flaffine3? t)  (flaffine3-determinant t)]
        [else            (flprojective3-determinant t)]))

(: flt3singular? (-> FlTransform3 Boolean))
(define (flt3singular? t)
  (define det (flt3determinant t))
  (or (= det 0.0)
      (= (abs (/ det)) +inf.0)))

(: flt3consistent? (-> FlTransform3 Boolean))
(define (flt3consistent? t)
  (cond [(fllinear3? t)  (fllinear3-consistent? t)]
        [(flaffine3? t)  (flaffine3-consistent? t)]
        [else            (flprojective3-consistent? t)]))

;; ===================================================================================================
;; Inversion

(: flt3inverse (case-> (-> FlLinear3     (U #f FlLinear3))
                       (-> FlAffine3     (U #f FlAffine3))
                       (-> FlAffLin3     (U #f FlAffLin3))
                       (-> FlProjective3 (U #f FlProjective3))
                       (-> FlTransform3  (U #f FlTransform3))))
(define (flt3inverse t)
  (cond [(fllinear3? t)  (fllinear3-inverse t)]
        [(flaffine3? t)  (flaffine3-inverse t)]
        [else            (flprojective3-inverse t)]))

;; ===================================================================================================
;; Application

(: flt3apply (-> FlTransform3 FlV4 FlV4))
(define (flt3apply t v)
  (cond [(fllinear3? t)  (fllinear3-apply t v)]
        [(flaffine3? t)  (flaffine3-apply t v)]
        [else            (flprojective3-apply t v)]))

(: flt3apply/pos (-> FlTransform3 FlV3 FlV3))
(define (flt3apply/pos t v)
  (cond [(fllinear3? t)  (fllinear3-apply/pos t v)]
        [(flaffine3? t)  (flaffine3-apply/pos t v)]
        [else            (flprojective3-apply/pos t v)]))

(: flt3apply/dir (-> FlTransform3 FlV3 FlV3))
(define (flt3apply/dir t v)
  (cond [(fllinear3? t)  (fllinear3-apply/dir t v)]
        [(flaffine3? t)  (flaffine3-apply/dir t v)]
        [else            (flprojective3-apply/dir t v)]))

(: flt3apply/norm (-> FlTransform3 FlV3 (U #f FlV3)))
(define (flt3apply/norm t v)
  (cond [(fllinear3? t)  (fllinear3-apply/norm t v)]
        [(flaffine3? t)  (flaffine3-apply/norm t v)]
        [else            (flprojective3-apply/norm t v)]))

(: flt3apply/plane (-> FlTransform3 FlPlane3 (U #f FlPlane3)))
(define (flt3apply/plane t p)
  (cond [(fllinear3? t)  (fllinear3-apply/plane t p)]
        [(flaffine3? t)  (flaffine3-apply/plane t p)]
        [else            (flprojective3-apply/plane t p)]))

;; ===================================================================================================
;; Composition

(: flt3compose (case-> (-> FlLinear3     FlLinear3     FlLinear3)
                       (-> FlAffLin3     FlAffine3     FlAffine3)
                       (-> FlAffine3     FlAffLin3     FlAffine3)
                       (-> FlAffLin3     FlAffLin3     FlAffLin3)
                       (-> FlProjective3 FlProjective3 FlProjective3)
                       (-> FlTransform3  FlProjective3 FlProjective3)
                       (-> FlProjective3 FlTransform3  FlProjective3)
                       (-> FlTransform3  FlTransform3  FlTransform3)))
(define (flt3compose m n)
  (cond [(fllinear3? m)
         (cond [(fllinear3? n)  (fllinear3-compose m n)]
               [(flaffine3? n)  (flaffine3-compose (->flaffine3 m) n)]
               [else            (flprojective3-compose (->flprojective3 m) n)])]
        [(flaffine3? m)
         (cond [(fllinear3? n)  (flaffine3-compose m (->flaffine3 n))]
               [(flaffine3? n)  (flaffine3-compose m n)]
               [else            (flprojective3-compose (->flprojective3 m) n)])]
        [else
         (cond [(fllinear3? n)  (flprojective3-compose m (->flprojective3 n))]
               [(flaffine3? n)  (flprojective3-compose m (->flprojective3 n))]
               [else            (flprojective3-compose m n)])]))
