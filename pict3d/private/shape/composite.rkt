#lang typed/racket/base

(require math/base
         math/flonum
         "../math.rkt"
         "../engine.rkt"
         "cylinder.rkt"
         "disk.rkt")

(provide make-arrow-scene)

(define 2pi (assert (* 2.0 pi) positive?))

(: make-arrow-scene (-> FlAffine3 Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                        FlV4 FlV4 FlV4 Boolean Nonempty-Scene))
;; Parameterized on transform, shaft radius fraction, arrow height fraction, indent height fraction
(define (make-arrow-scene arrow-t r h s c e m inside?)
  (define hrs (* h r s))
  
  (define shaft-t
    (flt3compose
     arrow-t
     (flt3compose
      (move-flt3 (flv3 0.0 0.0 (- (- h hrs))))
      (scale-flt3 (flv3 r r (- 1.0 (- h hrs)))))))
  
  (define head-t
    (flt3compose
     arrow-t
     (flt3compose
      (move-flt3 (flv3 0.0 0.0 (- 1.0 h)))
      (scale-flt3 (flv3 1.0 1.0 h)))))
  
  (define head
    (make-cylinder-shape head-t 0.0 2pi c e m inside?))
  
  (define base
    (cond [(> s epsilon.0)
           (define base-t
             (flt3compose
              arrow-t
              (flt3compose
               (move-flt3 (flv3 0.0 0.0 (+ (- 1.0 (* 2.0 h)) hrs)))
               (scale-flt3 (flv3 1.0 1.0 hrs)))))
           (make-cylinder-shape base-t r 2pi c e m (not inside?))]
          [else
           (make-disk-shape head-t r -1.0 2pi c e m (not inside?))]))
  
  (define shaft
    (nonempty-scene-union (make-cylinder-shape shaft-t 1.0 2pi c e m inside?)
                          (make-disk-shape shaft-t 0.0 -1.0 2pi c e m (not inside?))))
  
  (nonempty-scene-union (nonempty-scene-union head base) shaft))
