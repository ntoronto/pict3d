#lang typed/racket/base

(require racket/match
         racket/list
         "../utils.rkt"
         "types.rkt")

(provide (all-defined-out))

(: mesh->faces (All (A B) (-> (Vectorof vtx) (Vectorof Index) A B (Listof (face A B)))))
(define (mesh->faces vtxs idxs data edge-data)
  (for/list : (Listof (face A B)) ([i  (in-range 0 (vector-length idxs) 3)])
    (define vtx1 (vector-ref vtxs (vector-ref idxs i)))
    (define vtx2 (vector-ref vtxs (vector-ref idxs (+ i 1))))
    (define vtx3 (vector-ref vtxs (vector-ref idxs (+ i 2))))
    (face vtx1 vtx2 vtx3 data edge-data edge-data edge-data)))

(: faces->mesh (All (A B) (-> (Listof (face A B)) (Values (Vectorof vtx) (Vectorof Index)))))
(define (faces->mesh faces)
  (define vtxss
    (for/list : (Listof (Vectorof vtx)) ([f  (in-list faces)])
      (define-values (vtx1 vtx2 vtx3) (face-vtxs f))
      (vector vtx1 vtx2 vtx3)))
  
  (define idxss (make-list (length vtxss) ((inst vector Index) 0 1 2)))
  
  (indexed-vector-append vtxss idxss))
