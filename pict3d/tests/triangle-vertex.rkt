#lang typed/racket

(require pict3d
         math/flonum)

(: retesselate (-> (List Dir Dir Dir)
                   (Listof (List Dir Dir Dir))))
(define (retesselate vs)
  (match-define (list v0 v1 v2) vs)
  (define v01 (assert (dir-normalize (dir-scale (dir+ v0 v1) 0.5)) values))
  (define v12 (assert (dir-normalize (dir-scale (dir+ v1 v2) 0.5)) values))
  (define v20 (assert (dir-normalize (dir-scale (dir+ v2 v0) 0.5)) values))
  (list (list v0 v01 v20)
        (list v1 v12 v01)
        (list v2 v20 v12)
        (list v01 v12 v20)))

(define octahedron-dirs
  (list (list +x +y +z)
        (list +y -x +z)
        (list -x -y +z)
        (list -y +x +z)
        (list +y +x -z)
        (list -x +y -z)
        (list -y -x -z)
        (list +x -y -z)))

(: weird-material (-> Dir Material))
(define (weird-material dv)
  (match-define (dir dx dy dz) dv)
  (define d (expt (* 0.5 (+ dx 1.0)) 2))
  (define s (expt (- 1.0 (* 0.5 (+ dx 1.0))) 2))
  (define a (- 1.0 s d))
  (material #:ambient a
            #:diffuse d
            #:specular s
            #:roughness 0.1))

(: weird-color (-> Dir RGBA))
(define (weird-color dv)
  (match-define (dir dx dy dz) dv)
  (rgba (min 1.0 (+ 1.0 dx))
        (min 1.0 (+ 1.0 dy))
        (min 1.0 (+ 1.0 dz))))

(: weird-emitted (-> Dir Emitted))
(define (weird-emitted dv)
  (match-define (dir dx dy dz) dv)
  (emitted "white" (* 2.0 (max 0.0 (+ dx dy dz -1.25)))))

(: geodesic-sphere (-> Natural Pict3D))
(define (geodesic-sphere n)
  (define vss
    (for/fold ([vss : (Listof (List Dir Dir Dir))  octahedron-dirs]) ([_  (in-range n)])
      (append* (map retesselate vss))))
  (combine
   (map (λ ([vs : (List Dir Dir Dir)])
          (match-define (list dv0 dv1 dv2) vs)
          (triangle (vertex (pos+ origin dv0)
                            #:normal dv0
                            #:color (weird-color dv0)
                            #:emitted (weird-emitted dv0)
                            #:material (weird-material dv0))
                    (vertex (pos+ origin dv1)
                            #:normal dv1
                            #:color (weird-color dv1)
                            #:emitted (weird-emitted dv1)
                            #:material (weird-material dv1))
                    (vertex (pos+ origin dv2)
                            #:normal dv2
                            #:color (weird-color dv2)
                            #:emitted (weird-emitted dv2)
                            #:material (weird-material dv2))))
        vss)))

(freeze (geodesic-sphere 3))
