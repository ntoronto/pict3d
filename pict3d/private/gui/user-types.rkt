#lang typed/racket/base

(require racket/match
         typed/racket/class
         typed/racket/gui
         math/flonum
         "../engine/shape.rkt")

(provide User-Color
         User-Vector
         User-Material
         ->flv3
         ->flcolor4
         ->flcolor3
         ->material)

(define-type User-Color (U String Symbol (Sequenceof Real)))
(define-type User-Vector (Sequenceof Real))
(define-type User-Material (U material (List Real Real Real Real)))

(: ->flv3 (-> User-Vector FlVector))
(define (->flv3 xs)
  (define v (make-flvector 3 0.0))
  (for ([x  xs]
        [i  (in-range 3)])
    (flvector-set! v i (fl x)))
  v)

(: name->rgb (-> (U String Symbol) (Vector Byte Byte Byte)))
(define (name->rgb col)
  (let ([col  (if (symbol? col) (symbol->string col) col)])
    (define c (send the-color-database find-color col))
    (if c
        (vector (send c red) (send c green) (send c blue))
        (vector 0 0 0))))

(: ->flcolor4 (-> User-Color FlVector))
(define (->flcolor4 col)
  (cond [(or (symbol? col) (string? col))
         (match-define (vector r g b) (name->rgb col))
         (flvector (/ (fl r) 255.0)
                   (/ (fl g) 255.0)
                   (/ (fl b) 255.0)
                   1.0)]
        [else
         (define c (make-flvector 4 1.0))
         (for ([r  col]
               [i  (in-range 4)])
           (flvector-set! c i (fl r)))
         c]))

(: ->flcolor3 (-> User-Color FlVector))
(define (->flcolor3 col)
  (cond [(or (symbol? col) (string? col))
         (match-define (vector r g b) (name->rgb col))
         (flvector (/ (fl r) 255.0)
                   (/ (fl g) 255.0)
                   (/ (fl b) 255.0))]
        [else
         (define c (make-flvector 3 1.0))
         (for ([r  col]
               [i  (in-range 3)])
           (flvector-set! c i (fl r)))
         c]))

(: ->material (-> User-Material material))
(define (->material m)
  (match m
    [(? material? m)  m]
    [(list a d s r)  (material (fl a) (fl d) (fl s) (fl r))]))
