#lang typed/racket/base

(require racket/list
         racket/match
         math/flonum
         math/base
         pict3d
         "player.rkt"
         "debug-picts.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Player state

(struct moving
  ([linear-velocity : Dir]
   [angular-velocity : Dir]
   [jumping? : Boolean])
  #:transparent)

(struct pivoting
  ([offset : Dir]
   [axis : Dir]
   [speed : Flonum]
   [degrees : Flonum]
   [next-maneuver : maneuver])
  #:transparent)

(define-type maneuver (U moving pivoting))

(struct timers
  ([keys : (HashTable String Flonum)]
   [hits : (Listof (Pair Flonum Dir))]
   [jumps : (Listof (Pair Flonum Dir))])
  #:transparent)

(struct player-state
  ([position : Pos]
   [gravity : Dir]
   [rotation : Affine]
   [timers : timers]
   [maneuver : maneuver])
  #:transparent)

(define init-maneuver
  (moving zero-dir zero-dir #f))

(define init-timers
  (timers (make-immutable-hash) empty empty))

;; ===================================================================================================
;; Drawing

(: player-state->pict3d (-> player-state Pict3D))
(define (player-state->pict3d pstate)
  (match-define (player-state v ddv t _ man) pstate)
  (define t0 (affine-compose (move (pos- v origin)) t))
  (transform player t0))

;; ===================================================================================================
;; Physical constants

(define fps 60.0)
(define frame-delay (assert (/ 1000.0 fps) positive?))

;; Unless stated otherwise, constants use the following units:
;;  * Distance: meters
;;  * Time: seconds
;;  * Speed: meters/second
;;  * Acceleration: meters/second^2
;;  * Rotation: degrees

;; Minimum cube speed when sliding with no other forces acting on it
(define min-ground-speed 3.0)
;; Maximum cube speed under same circumstances
(define max-ground-speed 16.0)
;; Cube speed imparted by user direction (e.g. arrow keys)
(define user-speed 6.0)
;; Coefficient of sliding friction when cube speed is above min-speed
(define sliding-friction-coef 1.0)
;; Magnitude of gravity (direction may change)
(define gravity-accel 100.0)
;; Total rotation the cube should try for when it jumps
(define jump-spin-degrees 360.0)
;; Minimum distance of scene geometry from cube
(define offset-margin #i1/256)
;; Distance from cube center to any of its corners
(define player-radius
  (pos-dist origin (pos 0.5 0.5 0.5)))

(: restitution-coef (-> Dir Dir Flonum))
;; Scene-cube collision elasticity: 1 = elastic collisions, 0 = inelastic collisions
(define (restitution-coef ddv n)
  (define down (dir-normalize ddv))
  (cond [down
         ;; Make collisions with the ground perfecty inelastic, and those with walls bouncy
         (* 1.25 (- 1.0 (abs (dir-dot n down))))]
        [else
         ;; No down vector - better make them all inelastic
         0.0]))

;; Forced wait time between jumps (milliseconds)
(define jump-cooldown-time 300.0)
;; Forced wait time between wall bounce and arrowing back that direction (milliseconds)
(define arrow-cooldown-time 100.0)
;; Forced wait time between a jump and changing direction
(define jump->arrow-cooldown-time 25.0)

(: jump-mag (-> Dir Dir Flonum))
;; Jump magnitude; depends on velocity
(define (jump-mag dv ddv)
  (* 16.0 (+ 1.0 (/ (dir-dist (dir-reject dv ddv)) 16.0))))

(define min-jump-mag (jump-mag (dir-scale +x min-ground-speed) +x))

(define slide-jump-multiplier 1.25)

(: air-time (-> Dir Dir Flonum))
;; Expected time the cube will stay in the air after a jump, in seconds; depends on velocity
(define (air-time dv ddv)
  (* 2.0 (/ (jump-mag dv ddv) gravity-accel)))

(: jump-spin-speed (-> Dir Dir Flonum))
;; Speed the cube should spin, in radians/sec; depends on velocity
(define (jump-spin-speed dv ddv)
  (/ (degrees->radians jump-spin-degrees) (air-time dv ddv)))

(: physics-frames Positive-Integer)
;; Number of times per frame to advance physics along
;; Set to higher numbers to test framerate-independence
(define physics-frames 1)

;; ===================================================================================================

(: surface-data-normal* (-> Surface-Data Dir))
(define (surface-data-normal* data)
  (assert (surface-data-normal data) values))

(: angular-velocity->affine (-> Dir Flonum Affine))
(define (angular-velocity->affine da dtime)
  (let ([m  (dir-dist da)])
    (if (< m 1e-16)
        identity-affine
        (rotate (dir-scale da (/ 1.0 m)) (radians->degrees (* dtime m))))))

(: closest-axis (-> Dir Dir))
(define (closest-axis d)
  (match-define (dir dx dy dz) d)
  (define m (max (abs dx) (abs dy) (abs dz)))
  (cond [(= m (abs dx))  (dir-scale +x (sgn dx))]
        [(= m (abs dy))  (dir-scale +y (sgn dy))]
        [else            (dir-scale +z (sgn dz))]))

(: affine->arrow-axes (-> Affine Dir (Values Dir Dir)))
(define (affine->arrow-axes t down)
  (define-values (dx dy dz _) (affine->cols t))
  (values (closest-axis (dir-reject dx down))
          (closest-axis (dir-reject dy down))))

(: round-half (-> Flonum Flonum))
(define (round-half x)
  (* 0.5 (round (* 2.0 x))))

(: snap-position/velocity (-> Pos Dir Dir Pos))
(define (snap-position/velocity v dv ddv)
  (match-define (pos x y z) v)
  (match-define (dir dx dy dz) dv)
  (match-define (dir ddx ddy ddz) ddv)
  (define ddm (max (abs ddx) (abs ddy) (abs ddz)))
  (cond [(= ddm (abs ddx))
         (if (> (abs dy) (abs dz))
             (pos x y (round-half z))
             (pos x (round-half y) z))]
        [(= ddm (abs ddy))
         (if (> (abs dz) (abs dx))
             (pos (round-half x) y z)
             (pos x y (round-half z)))]
        [else
         (if (> (abs dx) (abs dy))
             (pos x (round-half y) z)
             (pos (round-half x) y z))]))

(: snap-position (-> Pos Dir Pos))
(define (snap-position v ddv)
  (match-define (pos x y z) v)
  (match-define (dir ddx ddy ddz) ddv)
  (define ddm (max (abs ddx) (abs ddy) (abs ddz)))
  (pos (if (= ddm (abs ddx)) x (round-half x))
       (if (= ddm (abs ddy)) y (round-half y))
       (if (= ddm (abs ddz)) z (round-half z))))

(: snap-velocity (-> Dir Dir Dir))
(define (snap-velocity dv ddv)
  (match-define (dir dx dy dz) dv)
  (match-define (dir ddx ddy ddz) ddv)
  (define ddm (max (abs ddx) (abs ddy) (abs ddz)))
  (cond [(= ddm (abs ddx))
         (cond [(= (abs dy) (abs dz))  (dir dx 0.0 0.0)]
               [(> (abs dy) (abs dz))  (dir dx  dy 0.0)]
               [else                   (dir dx 0.0  dz)])]
        [(= ddm (abs ddy))
         (cond [(= (abs dz) (abs dx))  (dir 0.0 dy 0.0)]
               [(> (abs dz) (abs dx))  (dir 0.0 dy  dz)]
               [else                   (dir  dx dy 0.0)])]
        [else
         (cond [(= (abs dx) (abs dy))  (dir 0.0 0.0 dz)]
               [(> (abs dx) (abs dy))  (dir  dx 0.0 dz)]
               [else                   (dir 0.0  dy dz)])]))

(: soft-clamp-component (-> Flonum Flonum Flonum))
(define (soft-clamp-component x dsecs)
  (cond [(= x 0.0)  0.0]
        [(< (abs x) min-ground-speed)  (* (sgn x) min-ground-speed)]
        [(> (abs x) max-ground-speed)  (* (sgn x) max-ground-speed)]
        [else
         (* (sgn x) (let ([x  (abs x)])
                      (max min-ground-speed (- x (* x sliding-friction-coef dsecs)))))]))

(: soft-clamp-velocity (-> Dir Dir Flonum Dir))
(define (soft-clamp-velocity dv ddv dsecs)
  (match-define (dir dx dy dz) dv)
  (match-define (dir ddx ddy ddz) ddv)
  (define ddm (max (abs ddx) (abs ddy) (abs ddz)))
  (dir (if (= ddm (abs ddx)) dx (soft-clamp-component dx dsecs))
       (if (= ddm (abs ddy)) dy (soft-clamp-component dy dsecs))
       (if (= ddm (abs ddz)) dz (soft-clamp-component dz dsecs))))

(: near-axis? (-> Dir Boolean))
(define (near-axis? d)
  (match-define (dir dx dy dz) d)
  (> (max (abs dx) (abs dy) (abs dz)) 0.9999))

(: near-axial? (-> Affine Boolean))
(define (near-axial? t)
  (define-values (dx dy dz p) (affine->cols t))
  (and (near-axis? dx) (near-axis? dy) (near-axis? dz)))

(: axisize (-> Dir Dir))
(define (axisize d)
  (match-define (dir dx dy dz) d)
  (define m (max (abs dx) (abs dy) (abs dz)))
  (cond [(= m (abs dx))  (dir (sgn dx) 0.0 0.0)]
        [(= m (abs dy))  (dir 0.0 (sgn dy) 0.0)]
        [else            (dir 0.0 0.0 (sgn dz))]))

(: axialize (-> Affine Affine))
(define (axialize t)
  (define-values (dx dy dz p) (affine->cols t))
  (cols->affine (axisize dx) (axisize dy) (axisize dz) p))

(: affine-nearest-axial (-> Affine Affine))
(define (affine-nearest-axial t)
  (define-values (dx dy dz p) (affine->cols t))
  (match-define (dir m00 m10 m20) dx)
  (match-define (dir m01 m11 m21) dy)
  (match-define (dir m02 m12 m22) dz)
  (define m0 (max (abs m00) (abs m10) (abs m20)))
  (define m1 (max (abs m01) (abs m11) (abs m21)))
  (define-values (ax ix)
    (cond [(= m0 (abs m00))  (values (dir (sgn m00) 0.0 0.0) 0)]
          [(= m0 (abs m10))  (values (dir 0.0 (sgn m10) 0.0) 1)]
          [else              (values (dir 0.0 0.0 (sgn m20)) 2)]))
  (define ay
    (cond [(and (= m1 (abs m01)) (not (= ix 0)))  (dir (sgn m01) 0.0 0.0)]
          [(and (= m1 (abs m11)) (not (= ix 1)))  (dir 0.0 (sgn m11) 0.0)]
          [(and (= m1 (abs m21)) (not (= ix 2)))  (dir 0.0 0.0 (sgn m21))]
          [(= ix 0)  (dir 0.0 (sgn m11) 0.0)]
          [(= ix 1)  (dir 0.0 0.0 (sgn m21))]
          [else      (dir (sgn m01) 0.0 0.0)]))
  (define az (dir-cross ax ay))
  (cols->affine ax ay az p))

(: affine->axis-angle (-> Affine (Values Dir Flonum)))
(define (affine->axis-angle t)
  (define-values (dx dy dz _) (affine->cols t))
  (match-define (dir m00 m10 m20) dx)
  (match-define (dir m01 m11 m21) dy)
  (match-define (dir m02 m12 m22) dz)
  (define θ (acos (max -1.0 (min 1.0 (* 0.5 (+ m00 m11 m22 -1.0))))))
  (define 2s (flsqrt (+ (sqr (- m21 m12)) (sqr (- m02 m20)) (sqr (- m10 m01)))))
  (define ax (/ (- m21 m12) 2s))
  (define ay (/ (- m02 m20) 2s))
  (define az (/ (- m10 m01) 2s))
  (values (dir ax ay az) θ))

(: axialize-angular (-> Affine Dir))
(define (axialize-angular t)
  (cond [(near-axial? t)  zero-dir]
        [else
         (define t0 (affine-nearest-axial t))
         (define-values (axis angle) (affine->axis-angle (affine-compose t (affine-inverse t0))))
         (define s (sgn angle))
         (define c (cos angle))
         (dir-scale axis (* s (* -2.0 pi) (acos (flexpt c 16.0))))]))

;; ===================================================================================================
;; Player collision detection

(define faces (list +x +y +z -x -y -z))

;; Physical cube face distances from center (visual are 0.5)
(define +s (- +0.5 offset-margin))
(define -s (- +s))

(define surface-offsets
  (for*/list : (Listof Dir) ([dx  (list -s 0.0 +s)]
                             [dy  (list -s 0.0 +s)]
                             [dz  (list -s 0.0 +s)]
                             #:when (= +s (max (abs dx) (abs dy) (abs dz))))
    (dir dx dy dz)))

(: find-offsets (-> (-> Dir Boolean) (Listof Dir)))
(define (find-offsets pred?)
  (filter pred? surface-offsets))

(: edge-offsets (Listof (Listof Dir)))
(define edge-offsets
  (list (find-offsets (λ (d) (and (= (dir-dx d) +s) (= (dir-dy d) +s))))
        (find-offsets (λ (d) (and (= (dir-dx d) +s) (= (dir-dy d) -s))))
        (find-offsets (λ (d) (and (= (dir-dx d) -s) (= (dir-dy d) +s))))
        (find-offsets (λ (d) (and (= (dir-dx d) -s) (= (dir-dy d) -s))))
        (find-offsets (λ (d) (and (= (dir-dy d) +s) (= (dir-dz d) +s))))
        (find-offsets (λ (d) (and (= (dir-dy d) +s) (= (dir-dz d) -s))))
        (find-offsets (λ (d) (and (= (dir-dy d) -s) (= (dir-dz d) +s))))
        (find-offsets (λ (d) (and (= (dir-dy d) -s) (= (dir-dz d) -s))))
        (find-offsets (λ (d) (and (= (dir-dz d) +s) (= (dir-dx d) +s))))
        (find-offsets (λ (d) (and (= (dir-dz d) +s) (= (dir-dx d) -s))))
        (find-offsets (λ (d) (and (= (dir-dz d) -s) (= (dir-dx d) +s))))
        (find-offsets (λ (d) (and (= (dir-dz d) -s) (= (dir-dx d) -s))))))

(define corner-offsets
  (for*/list : (Listof Dir) ([dx  (list -s +s)]
                             [dy  (list -s +s)]
                             [dz  (list -s +s)])
    (dir dx dy dz)))

(: trace/offset (-> Pict3D Pos Affine Dir Affine Dir (U #f Surface-Data)))
(define (trace/offset coll-pict v t delta-v rot-t offset)
  (define v1 (pos+ v (transform-dir offset t)))
  (define v2 (pos+ v (dir+ delta-v (transform-dir offset (affine-compose rot-t t)))))
  (trace/data coll-pict v1 v2))

(: trace/player (-> Pict3D Pos Affine Dir Affine (Values Flonum (Listof Surface-Data))))
(define (trace/player coll-pict v t delta-v rot-t)
  (define all-datas
    (for/fold ([datas : (Listof Surface-Data)  empty]) ([offset  (in-list surface-offsets)])
      (define data (trace/offset coll-pict v t delta-v rot-t offset))
      (if (and data (surface-data-normal data))
          (cons data datas)
          datas)))
  
  (define dist (apply min 1.0 (map surface-data-dist all-datas)))
  (define datas (filter (λ ([data : Surface-Data]) (= dist (surface-data-dist data))) all-datas))
  (values dist datas))

(: trace-face (-> Pict3D Pos Dir Dir (Listof (U #f Pos))))
(define (trace-face coll-pict v d up)
  (define left (dir-normalize (dir-cross d up)))
  (cond
    [left
     (for*/list : (Listof (U #f Pos)) ([sx  (in-list '(-0.25 +0.25))]
                                       [sy  (in-list '(-0.25 +0.25))])
       (let ([v1  (pos+ v (dir+ (dir-scale up sy) (dir-scale left sx)))])
         (trace coll-pict v1 (pos+ v1 d))))]
    [else
     (list #f #f #f #f)]))

;; ===================================================================================================
;; Player physics

;; ---------------------------------------------------------------------------------------------------
;; Jumping

(define dir-near?
  (λ ([d1 : Dir] [d2 : Dir])
    (> (dir-dot d1 d2) 0.9999)))

(: jump-normal? (-> Flonum Dir (Listof (Pair Flonum Dir)) Boolean))
(define (jump-normal? time n jumps)
  (let loop ([jumps jumps])
    (cond [(empty? jumps)  #t]
          [else
           (define tn (first jumps))
           (cond  [(and (<= (- time (car tn)) jump-cooldown-time)
                        (dir-near? n (cdr tn)))
                   #f]
                  [else  (loop (rest jumps))])])))

(: find-jump-dir (-> Pict3D Flonum Flonum Pos Dir Dir
                     (Listof (Pair Flonum Dir))
                     (Listof (Pair Flonum Dir))
                     (Values (U #f Dir) (Listof (Pair Flonum Dir)))))
(define (find-jump-dir coll-pict time mag v dv down hits jumps)
  ;(printf "jump: hits = ~v~n" hits)
  
  ;; Consider every normal we hit on this frame as a jump direction
  (define max-time (apply max -inf.0 (map (inst car Flonum Dir) hits)))
  (define ns
    (for/fold ([ns : (Listof Dir)  empty]) ([tn  (in-list hits)])
      (if (and (= max-time (car tn))
               (jump-normal? time (cdr tn) jumps))
          (cons (cdr tn) ns)
          ns)))
  
  ;; Consider the normals of surfaces currently touching that are perpendicular to a normal
  (define slide-ns
    (remove-duplicates
     (for/fold ([slide-ns : (Listof Dir)  empty])
               ([face  (in-list faces)])
       (define face-n (dir-negate face))
       (define n (ormap (λ ([n : Dir]) (and (< (abs (dir-dot face-n n)) 1e-8) n)) ns))
       (cond [(not n)  slide-ns]
             [(not (jump-normal? time n jumps))  slide-ns]
             [else
              (define ps (trace-face coll-pict v (dir-scale face 0.51) n))
              (define p (ormap (λ ([p : (U #f Pos)]) p) ps))
              (cond [(not p)  slide-ns]
                    [else  (list* n face-n slide-ns)])]))
     dir-near?))
  
  (define bounce-ns (remove* slide-ns ns dir-near?))
  #;
  (add-debug-pict!
   (combine
    (with-emitted (emitted 1 0.5 1 3)
      (combine
       (for/list : (Listof Pict3D) ([n  (in-list slide-ns)])
         (arrow v (dir-scale n 0.5)))))
    (with-emitted (emitted 0.5 1 0.5 3)
      (combine
       (for/list : (Listof Pict3D) ([n  (in-list bounce-ns)])
         (arrow v (dir-scale n 0.5))))))
   2000.0)
  
  (define slide-dv
    (for/fold ([nsum : Dir  zero-dir]) ([n  (in-list slide-ns)])
      (dir+ nsum (dir-scale n (* slide-jump-multiplier mag)))))
  
  (define len (fl (length bounce-ns)))
  (define bounce-dv
    (for/fold ([nsum : Dir  zero-dir]) ([n  (in-list bounce-ns)])
      ;; Find out how much it already will have bounced
      (define c (min 1.0 (restitution-coef down n)))
      (dir+
       ;; Add jump in the direction of the normal for unbouncy surfaces
       (dir-scale n (* (/ mag len) (- 1.0 c)))
       ;; Add jump in the up direction for bouncy surfaces
       (dir-scale down (* (- (/ min-jump-mag len)) c)))))
  #;
  (add-debug-pict!
   (combine
    (if (> (dir-dist slide-dv) 0.0)
        (with-emitted (emitted 0.5 1 1 3)
          (arrow v (dir-scale slide-dv 0.125)))
        empty-pict3d)
    (if (> (dir-dist bounce-dv) 0.0)
        (with-emitted (emitted 1 1 0.5 3)
          (arrow v (dir-scale bounce-dv 0.125)))
        empty-pict3d))
   2000.0)
  
  (values (dir+ slide-dv bounce-dv)
          (map (λ ([n : Dir]) (cons time n)) (append slide-ns bounce-ns))))

(: player-state-maybe-jump (-> player-state Pict3D Flonum Flonum player-state))
(define (player-state-maybe-jump pstate coll-pict time dsecs)
  (match-define (player-state v ddv t (timers keys hits jumps) man) pstate)
  (cond
    [(and (> (hash-ref keys " " (λ () 0.0)) 0.0)
          (not (empty? hits)))
     (match man
       [(moving dv da #f)
        (define down (let ([down  (dir-normalize ddv)])
                       (if down down zero-dir)))
        (define mag (jump-mag dv ddv))
        (define-values (jump-dir new-jumps) (find-jump-dir coll-pict time mag v dv down hits jumps))
        (cond [jump-dir
               (define axis (let ([axis  (dir-normalize (dir-cross jump-dir dv))])
                              (if axis axis zero-dir)))
               (define new-dv (snap-velocity (dir+ dv jump-dir) ddv))
               #;
               (when (> (dir-dist new-dv) 0.0)
                 (add-debug-pict!
                  (with-emitted (emitted 1 3)
                    (arrow v (dir-scale new-dv 0.125)))
                  2000.0))
               (define new-da (dir-scale axis (jump-spin-speed dv ddv)))
               (define new-man (moving new-dv new-da #t))
               (define new-tm (timers keys hits (append new-jumps jumps)))
               (player-state v ddv t new-tm new-man)]
              [else  pstate])]
       [_  pstate])]
    [else  pstate]))

;; ---------------------------------------------------------------------------------------------------
;; Arrowing

(: player-state-maybe-arrow (-> player-state Pict3D Flonum Flonum String Dir Dir player-state))
(define (player-state-maybe-arrow pstate coll-pict time dsecs key req-d last-dv)
  (match-define (player-state v ddv t (and tm (timers keys hits jumps)) man) pstate)
  (cond
    [(> (hash-ref keys key (λ () 0.0)) 0.0)
     (match man
       [(moving dv da jumping?)
        ;(printf "arrow: hits = ~v~n" hits)
        (define d
          (for/fold ([d : Dir  req-d]) ([tn  (in-list hits)])
            (if (<= (- time (car tn)) arrow-cooldown-time)
                (dir-reject d (cdr tn) 0.5)
                d)))
        (cond
          [(not d)  #f]
          [(ormap (λ ([tn : (Pair Flonum Dir)])
                    (<= (- time (car tn)) jump->arrow-cooldown-time))
                  jumps)
           pstate]
          [else
           (define down (assert (dir-normalize ddv) values))
           ;; Vertical velocity
           (define dz (assert (dir-project dv down) values))
           ;; Horizontal velocities
           (define dh (dir- dv dz))
           (define speed (dir-dist dh))
           (define new-dv (dir+ dz (dir-scale d (max user-speed speed))))
           #;
           (when (> (dir-dist new-dv) 0.0)
             (add-debug-pict!
              (with-emitted (emitted 1 3)
                (arrow v (dir-scale new-dv 0.125)))
              2000.0))
           (player-state v ddv t tm (moving new-dv da jumping?))])]
       [_  pstate])]
    [else  pstate]))

;; ---------------------------------------------------------------------------------------------------
;; Edge pivoting

(: player-state-maybe-edge-pivot (-> player-state Pict3D Flonum Flonum player-state))
(define (player-state-maybe-edge-pivot pstate coll-pict time dsecs)
  (match pstate
    [(player-state v ddv (? near-axial?) (timers keys hits jumps)
                   (moving (? (λ (dv) (>= (dir-dist dv) 1.0)) dv) da jumping?))
     (define face (argmax (λ ([face : Dir]) (dir-dot dv face)) faces))
     (define center-offset (dir-scale face (- 0.5 offset-margin)))
     (set! v (snap-position v ddv))
     (define-values (score edge-offset)
       (for/fold ([best-score : Flonum  +inf.0]
                  [best-edge-offset : Dir  zero-dir])
                 ([offsets  (in-list edge-offsets)])
         (define edge-offset (second offsets))
         (cond
           [(or (ormap (λ ([offset : Dir]) (< (dir-dot offset face) 0.1)) offsets)
                (ormap (λ ([offset : Dir])
                         (define v1 (pos+ v (dir- (dir-scale center-offset 1.5)
                                                  (dir-scale offset 0.5))))
                         (define v2 (pos+ v1 (dir-scale face 0.25)))
                         (trace coll-pict v1 v2))
                       offsets))
            (values best-score best-edge-offset)]
           [else
            ;(printf "offsets = ~v~n" offsets)
            (define datas
              (for/fold ([datas : (Listof Surface-Data)  empty]) ([offset  (in-list offsets)])
                (define v0 (pos+ v (dir+ (dir-scale center-offset 0.5)
                                         (dir-scale offset 0.5))))
                (define data (trace/data coll-pict v0 (pos+ v0 (dir-scale face 0.25))))
                (if (and data (surface-data-normal data)) (cons data datas) datas)))
            (cond
              [(< (length datas) 3)
               (values best-score best-edge-offset)]
              [else
               (define ns (map surface-data-normal* datas))
               (define n (assert (dir-normalize (dir+ (dir+ (first ns) (second ns)) (third ns)))
                                 values))
               (cond
                 [(or (> (dir-dot face n) (cos (degrees->radians 135.0)))
                      (> (/ (dir-dot dv n) (dir-dist dv)) (cos (degrees->radians 135.0))))
                  (values best-score best-edge-offset)]
                 [else
                  (define score (dir-dot face n))
                  (if (< score best-score)
                      (values score edge-offset)
                      (values best-score best-edge-offset))])])])))
     
     (define speed (* (/ 2.0 min-ground-speed) pi (max min-ground-speed (dir-dist dv))))
     (cond
       [(= score +inf.0)  pstate]
       [else
        (define axis (assert (dir-normalize (dir-cross face edge-offset)) values))
        (define rot-offset (dir-scale face 0.5))
        (printf "edge pivot: ~v ~v~n" axis rot-offset)
        (define new-pivot (pivoting rot-offset axis speed (* 0.5 pi) (moving dv zero-dir #f)))
        (define new-timers (timers keys empty empty))
        (player-state v ddv identity-affine new-timers new-pivot)])]
    [_  pstate]))

;; ---------------------------------------------------------------------------------------------------
;; Corner pivoting

(: player-state-maybe-corner-pivot (-> player-state Pict3D Flonum Flonum player-state))
(define (player-state-maybe-corner-pivot pstate coll-pict time dsecs)
  (match pstate
    [(player-state v ddv (? near-axial?) (timers keys hits jumps)
                   (moving (? (λ (dv) (>= (dir-dist dv) 1.0)) dv) da jumping?))
     (define face (argmax (λ ([face : Dir]) (dir-dot dv face)) faces))
     (define center-offset (dir-scale face (- 0.5 offset-margin)))
     (set! v (snap-position v ddv))
     (define-values (score offset)
       (for/fold ([best-score : Flonum  +inf.0]
                  [best-offset : Dir  zero-dir])
                 ([offset  (in-list corner-offsets)])
         (define rot-t (rotate face 90))
         (define offset1 (transform-dir offset rot-t))
         (define offset2 (transform-dir offset1 rot-t))
         (define offset3 (transform-dir offset2 rot-t))
         (cond
           [(or (< (dir-dot offset face) 0.1)
                (ormap (λ ([offset : Dir])
                         (define v0  (pos+ v (dir+ (dir-scale center-offset 0.5)
                                                   (dir-scale offset 0.5))))
                         (trace coll-pict v0 (pos+ v0 (dir-scale face 0.25))))
                       (list offset1 offset2 offset3)))
            (values best-score best-offset)]
           [else
            ;(printf "offsets = ~v~n" offsets)
            (define data
              (let ([v0  (pos+ v (dir+ (dir-scale center-offset 0.5)
                                       (dir-scale offset 0.5)))])
                (trace/data coll-pict v0 (pos+ v0 (dir-scale face 0.25)))))
            (cond
              [(not (and data (surface-data-normal data)))
               (values best-score best-offset)]
              [else
               (define n (surface-data-normal* data))
               (cond
                 [(or (> (dir-dot face n) (cos (degrees->radians 135.0)))
                      (> (/ (dir-dot dv n) (dir-dist dv)) (cos (degrees->radians 135.0))))
                  (values best-score best-offset)]
                 [else
                  (define score (dir-dot face n))
                  (if (< score best-score)
                      (values score offset)
                      (values best-score best-offset))])])])))
     
     (define speed (* (/ 2.0 min-ground-speed) pi (max min-ground-speed (dir-dist dv))))
     (cond
       [(= score +inf.0)  pstate]
       [else
        (define axis (assert (dir-normalize (dir-cross face offset)) values))
        (define rot-offset (dir-scale face 0.5))
        (printf "corner pivot: ~v ~v~n" axis rot-offset)
        (define new-pivot (pivoting rot-offset axis speed (* 0.5 pi) (moving dv zero-dir #f)))
        (define new-timers (timers keys empty empty))
        (player-state v ddv identity-affine new-timers new-pivot)])]
    [_  pstate]))

;; ---------------------------------------------------------------------------------------------------
;; Maneuver normalization

(: player-state-normalize (-> player-state Flonum player-state))
(define (player-state-normalize pstate time)
  ;(printf "new frame~n")
  (match-define (player-state v ddv t (timers keys hits jumps) man) pstate)
  (define axial? (near-axial? t))
  (define new-t (if axial? (axialize t) t))
  (define new-jumps (filter (λ ([tn : (Pair Flonum Dir)]) (<= (- time (car tn)) 500.0)) jumps))
  (define new-hits  (filter (λ ([tn : (Pair Flonum Dir)]) (<= (- time (car tn)) 500.0)) hits))
  (define new-timers (timers keys new-hits new-jumps))
  (define new-man
    (match man
      [(moving dv da jumping?)
       (define speed (dir-dist dv))
       (define new-dv (if (< speed 0.1) zero-dir dv))
       (define new-da (if (and axial? (< (dir-dist da) 0.1)) zero-dir da))
       (moving new-dv new-da jumping?)]
      [_  man]))
  (player-state v ddv new-t new-timers new-man))

;; ---------------------------------------------------------------------------------------------------
;; Movement

(: player-state-move (-> player-state Pict3D Flonum Flonum player-state))
(define (player-state-move pstate coll-pict time dsecs)
  (match-define (player-state v ddv t tm man) pstate)
  ;(printf "dsecs = ~v~n" dsecs)
  (match man
    [(moving dv da jumping?)
     (define delta-v (dir+ (dir-scale dv dsecs) (dir-scale ddv (* 0.5 (sqr dsecs)))))
     (define rot-t (angular-velocity->affine da dsecs))
     (define-values (dist datas) (trace/player coll-pict v t delta-v rot-t))
     (define num-hits (length datas))
     ;; The maximum distance a corner can travel due to rotation
     (define max-corner-dist (* (dir-dist da) player-radius dsecs))
     ;; If max-corner-dist > offset-margin; i.e. the maximum distance a corner can rotate is greater
     ;; than the smallest allowable distance between the cube and scene geometry, then we need to run
     ;; physics in subdivided time to keep from putting any part of the cube inside of scene geometry
     ;; when we partially rotate the cube
     (cond
       [(and (< dist 1.0) (> max-corner-dist offset-margin))
        (let ([pstate  (player-state-move pstate coll-pict time (* dsecs 0.5))])
          (player-state-move pstate coll-pict time (* dsecs 0.5)))]
       [(< dist 1.0)
        (define part-dsecs (* dist dsecs))
        
        (define part-v (pos+ v delta-v dist))
        (define part-rot-t (angular-velocity->affine da part-dsecs))
        (define part-dv (dir+ dv (dir-scale ddv part-dsecs)))
        (define part-da da)
        
        ;; React to every surface
        (define add-dv
          (for/fold ([add-dv : Dir  zero-dir])
                    ([data  (in-list datas)])
            (define n (surface-data-normal* data))
            (define r (pos- (surface-data-pos data) part-v))
            (define total-dv (dir+ part-dv (dir-cross part-da r)))
            ;; Leaving out the angular part (the denominator) because this is all going to velocity:
            (define j (* (dir-dot total-dv n) (- (+ 1.0 (restitution-coef ddv n)))))
            ;; Reject this normal a bit
            (dir+ add-dv (dir-scale n (/ j (fl num-hits))))))
        
        ;; Offset the position in the direction of every normal
        (define ns (remove-duplicates (map surface-data-normal* datas)))
        (define add-v
          (for/fold ([add-v : Dir  (dir-scale delta-v dist)]) ([n  (in-list ns)])
            (dir+ add-v (dir-scale n offset-margin))))
        
        (define next-dv (soft-clamp-velocity (dir+ part-dv add-dv) ddv part-dsecs))
        (define next-v (snap-position/velocity (pos+ v add-v) next-dv ddv))
        (define next-t (affine-compose part-rot-t t))
        (define next-da (axialize-angular next-t))
        
        (match-define (timers keys hits jumps) tm)
        (define next-hits (remove-duplicates (append (map (λ ([n : Dir]) (cons time n)) ns) hits)))
        (define next-tm (timers keys next-hits jumps))
        
        (define next-man (moving next-dv next-da #f))
        
        (define next-pstate (player-state next-v ddv next-t next-tm next-man))
        
        ;; Only continue moving this frame if he didn't hit a wall
        (define continue-move?
          (andmap (λ ([n : Dir]) (> (/ (dir-dot n dv) (dir-dist dv)) (cos (degrees->radians 135))))
                  ns))
        
        (if continue-move?
            (player-state-move next-pstate coll-pict time (- dsecs part-dsecs))
            next-pstate)]
       [else
        (define next-dv
          (let ([next-dv  (dir+ dv (dir-scale ddv dsecs))])
            (if jumping? next-dv (soft-clamp-velocity next-dv ddv dsecs))))
        (define next-v (snap-position/velocity (pos+ v delta-v) next-dv ddv))
        (define next-t (affine-compose rot-t t))
        (define next-man (moving next-dv da jumping?))
        (player-state next-v ddv next-t tm next-man)])]
    [(pivoting offset axis speed deg next-man)
     (cond
       [(< deg 1e-8)
        (printf "deg ≈ 0: pivot done~n")
        ;; Don't continue the move - needs a chance to detect another pivot
        (player-state (snap-position v ddv) ddv t tm next-man)]
       [else
        (define toffset (transform-dir offset t))
        (define taxis   (transform-dir axis t))
        (define delta-a (min deg (* speed dsecs)))
        (define rot-t (rotate taxis (radians->degrees delta-a)))
        (define delta-v (dir+ toffset (transform-dir (dir-negate toffset) rot-t)))
        (define-values (dist datas) (trace/player coll-pict v t delta-v rot-t))
        (cond
          [(not (empty? datas))
           (printf "collision: pivot done~n")
           (player-state-move (player-state v ddv t tm next-man)
                              coll-pict time dsecs)]
          [else
           (let ([v  (pos+ v delta-v)]
                 [t  (affine-compose rot-t t)])
             (define pivot-man (pivoting offset axis speed (- deg delta-a) next-man))
             (player-state v ddv t tm pivot-man))])])]))

;; ===================================================================================================
;; Main player state loop body

(: camera-offset (-> Dir Dir))
(define (camera-offset ddv)
  (match (dir-normalize ddv)
    [(dir dx dy dz)
     (define m (max (abs dx) (abs dy) (abs dz)))
     (cond [(= m (abs dx))  (dir (* -6 (sgn dx)) -6 0.0)]
           [(= m (abs dy))  (dir 0.0 -6 0.0)]
           [else            (dir 0.0 -6 (* -6 (sgn dz)))])]
    [_  (dir 5 5 5)]))

(: player-state-advance (-> player-state Pict3D Flonum Flonum player-state))
(define (player-state-advance pstate coll-pict time dsecs)
  (match-define (player-state v ddv t tm man) pstate)
  
  (define down (assert (dir-normalize ddv) values))
  (define look-t (point-at (pos+ v (camera-offset ddv)) v))
  (define-values (+dx +dy) (affine->arrow-axes look-t down))
  (define -dx (dir-negate +dx))
  (define -dy (dir-negate +dy))
  (define dv (if (moving? man) (moving-linear-velocity man) zero-dir))
  
  (let* ([pstate  (player-state-maybe-jump pstate coll-pict time dsecs)]
         [pstate  (player-state-maybe-arrow pstate coll-pict time dsecs "left" -dx dv)]
         [pstate  (player-state-maybe-arrow pstate coll-pict time dsecs "right" +dx dv)]
         [pstate  (player-state-maybe-arrow pstate coll-pict time dsecs "up" -dy dv)]
         [pstate  (player-state-maybe-arrow pstate coll-pict time dsecs "down" +dy dv)]
         [pstate  (player-state-normalize pstate time)]
         [pstate  (player-state-maybe-edge-pivot pstate coll-pict time dsecs)]
         [pstate  (player-state-maybe-corner-pivot pstate coll-pict time dsecs)]
         [pstate  (for/fold ([pstate : player-state  pstate]) ([_  (in-range physics-frames)])
                    (player-state-move pstate coll-pict time (/ dsecs (fl physics-frames))))])
    pstate))

(: player-state-on-key (-> player-state Flonum String player-state))
(define (player-state-on-key pstate time key)
  (match-define (player-state v ddv t (timers keys hits jumps) man) pstate)
  (let ([keys  (hash-set keys key time)])
    (player-state v ddv t (timers keys hits jumps) man)))

(: player-state-on-release (-> player-state Flonum String player-state))
(define (player-state-on-release pstate time key)
  (match-define (player-state v ddv t (timers keys hits jumps) man) pstate)
  (let ([keys  (hash-remove keys key)])
    (player-state v ddv t (timers keys hits jumps) man)))
