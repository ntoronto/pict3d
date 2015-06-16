#lang racket

(require pict3d
         "check-ray-trace-vs-opengl.rkt"
         "check-tessellate-vs-orig.rkt")

;; This was taking WAY too long
(time
 (void
  (tessellate
   (scale
    (group
     (scale
      (quad (pos 0 (- 0.125) 0) (pos 0.5 (- 1.0) 0) (pos 0 0.25 0) (pos 0.5 0 0) #:back? #t)
      (dir 0.5 0.0625 (- 0.125)))
     'E)
    (dir 0.0625 0.0625 (- 0.25))))))

;; vector-ref: index is out of range\n  index: 2\n  valid range: [0, 1]
(check-tessellate-vs-orig
 (group
  (scale
   (scale
    (freeze (pipe (pos 0 0 0) (dir 0.0625 0.25 (- 0.125))
                  #:bottom-radii (interval 0.5 0.5)
                  #:top-radii (interval 0.0625 0.5)
                  #:arc (arc 195 120)
                  #:inside? #f))
    (dir (- 0.25) 0.5 (- 0.125)))
   (dir (- 0.25) (- 1.0) (- 0.25)))
  'T))

(check-tessellate-vs-orig
 (cone (pos 1/2 0 0) (dir 1/4 1/16 1/16) #:arc (arc 315 195) #:inside? #t))

(check-tessellate-vs-orig
 (pipe (pos (- 1) 0 (- 1/2))
       (dir 1/4 (- 1/4) (- 1/16))
       #:bottom-radii (interval 0 0)
       #:top-radii (interval 1 0)
       #:arc (arc 240 330)
       #:inside? #t))

(check-tessellate-vs-orig
 (scale
  (scale
   (rotate
    (quad (pos (- 1/8) 0 0)
          (pos (- 1/16) 0 1/4)
          (pos 1/4 0 0)
          (pos 0 (- 1/4) 0)
          #:back? #f)
    (dir 1/16 1/4 (- 1/16))
    75)
   (dir (- 1) (- 1) 1))
  (dir 1 (- 1) 1)))

;; This was taking WAY too long
(time
 (void
  (tessellate
   (scale
    (rotate
     (group
      (scale
       (cylinder (pos 0 0 0) (dir (- 1) (- 1/4) 1/16)
                 #:arc (arc 210 210)
                 #:inside? #f)
       (dir 1/16 (- 1/4) (- 1/8)))
      'v)
     (dir 1/16 (- 1/8) 1/16)
     330)
    (dir (- 1/16) (- 1/8) (- 1/2))))))

(check-ray-trace-vs-opengl
 (scale-y
  (rotate-y
   (combine
    (scale-z
     (rotate-z
      (freeze
       (group
        (cylinder (pos 0 0 1/4) (dir (- 1/2) (- 1) 1/16) #:inside? #t)
        'o))
      165)
     (- 1/16))
    (freeze (rectangle (pos 0 0 0) (dir 1/8 1/2 1/2) #:inside? #f))) 255)
  (- 1/2)))

(check-ray-trace-vs-opengl
 (rotate-x
  (group
   (group
    (scale-z
     (ellipsoid (pos 1/8 0 0) (dir 1 1/2 1/16) #:inside? #f)
     (- 1/16))
    'D)
   'j)
  300))

(check-ray-trace-vs-opengl
 (rotate
  (cylinder (pos 1/4 (- 1/8) 1/4) (dir (- 1/8) 1/16 1/16) #:arc (arc 135 330) #:inside? #t)
  (dir 1/8 (- 1/2) 1/16)
  225))

(check-ray-trace-vs-opengl
 (freeze
  (freeze
   (rotate
    (freeze
     (scale
      (cylinder (pos 0 1/2 1/8) (dir 1/16 1/2 1/16) #:arc (arc 270 210) #:inside? #t)
      (dir 1/16 1/4 (- 1))))
    (dir 1/8 (- 1/2) (- 1/8))
    240))))
