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

(: restitution-coef (-> Dir Dir Boolean Flonum))
;; Scene-cube collision elasticity: 1 = elastic collisions, 0 = inelastic collisions
(define (restitution-coef ddv n jumping?)
  (define down (dir-normalize ddv))
  (cond [down
         ;; Make collisions with the ground perfecty inelastic, and those with walls bouncy
         (* (if jumping? 1.0 1.25) (- 1.0 (abs (dir-dot n down))))]
        [else
         ;; No gravity - better make them all inelastic
         0.0]))

;; Forced wait time between jumps
(define jump-cooldown-time 100.0)
;; Forced wait time between a jump and responding to an arrow key
(define jump->arrow-cooldown-time 200.0)

(: jump-mag (-> Dir Dir Flonum))
;; Jump magnitude; depends on velocity
(define (jump-mag dv ddv)
  (* 16.0 (+ 1.0 (/ (dir-dist (dir-reject dv ddv)) 16.0))))

(define min-jump-mag (jump-mag (dir-scale +x min-ground-speed) +x))

(define slide-jump-multiplier 1.75)

(: air-time (-> Dir Dir Flonum))
;; Expected time the cube will stay in the air after a jump, in seconds; depends on velocity
(define (air-time dv ddv)
  (* 2.0 (/ (jump-mag dv ddv) gravity-accel)))

(: jump-spin-speed (-> Dir Dir Flonum))
;; Speed the cube should spin, in radians/sec; depends on velocity
(define (jump-spin-speed dv ddv)
  (/ (degrees->radians jump-spin-degrees) (air-time dv ddv)))

(define min-pivot-speed 1.0)

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
        (rotate (dir-scale da (/ m)) (radians->degrees (* dtime m))))))

(: closest-axis (-> Dir Dir))
(define (closest-axis d)
  (match-define (dir dx dy dz) d)
  (define m (max (abs dx) (abs dy) (abs dz)))
  (cond [(= m (abs dx))  (dir-scale +x (sgn dx))]
        [(= m (abs dy))  (dir-scale +y (sgn dy))]
        [else            (dir-scale +z (sgn dz))]))

(: affine->arrow-axes (-> Affine Dir (Values Dir Dir)))
(define (affine->arrow-axes t down)
  (values (closest-axis (dir-reject (affine-x-axis t) down))
          (closest-axis (dir-reject (affine-y-axis t) down))))

(: round-half (-> Flonum Flonum))
(define (round-half x)
  (* 0.5 (round (* 2.0 x))))

(: delta->velocity (-> Flonum Flonum Flonum))
(define (delta->velocity d mx)
  (define v (/ d #i1/60))
  (if (> (abs v) (abs mx))
      (* (sgn v) (abs mx))
      v))

(: align-velocity (-> Pos Dir Dir Flonum Dir))
;; Zero out one or two velocity components, depending on the direction of gravity
(define (align-velocity v dv ddv dsecs)
  (match-define (pos x y z) v)
  (match-define (dir dx dy dz) dv)
  (match-define (dir ddx ddy ddz) ddv)
  (define ddm (max (abs ddx) (abs ddy) (abs ddz)))
  (define new-dv
    (cond [(= ddm (abs ddx))
           (if (> (abs dy) (abs dz))
               (dir dx dy (delta->velocity (- (round-half z) z) (* dy 0.5)))
               (dir dx (delta->velocity (- (round-half y) y) (* dz 0.5)) dz))]
          [(= ddm (abs ddy))
           (if (> (abs dz) (abs dx))
               (dir (delta->velocity (- (round-half x) x) (* dx 0.5)) dy dz)
               (dir dx dy (delta->velocity (- (round-half z) z) (* dz 0.5))))]
          [else
           (if (> (abs dx) (abs dy))
               (dir dx (delta->velocity (- (round-half y) y) (* dx 0.5)) dz)
               (dir (delta->velocity (- (round-half x) x) (* dy 0.5)) dy dz))]))
  new-dv)

(: soft-clamp-component (-> Flonum Flonum Flonum))
;; Keep the cube moving, but not too fast
(define (soft-clamp-component x dsecs)
  (cond [(< (abs x) min-ground-speed)  (* (sgn x) min-ground-speed)]
        [(> (abs x) max-ground-speed)  (* (sgn x) max-ground-speed)]
        [else
         (* (sgn x) (let ([x  (abs x)])
                      (max min-ground-speed (- x (* x sliding-friction-coef dsecs)))))]))

(: soft-clamp-velocity (-> Dir Dir Flonum Dir))
(define (soft-clamp-velocity dv ddv dsecs)
  (match-define (dir dx dy dz) dv)
  (match-define (dir ddx ddy ddz) ddv)
  (define ddm (max (abs ddx) (abs ddy) (abs ddz)))
  (define new-dv
    (cond [(= ddm (abs ddx))
           (define d (dir 0.0 dy dz))
           (define m (dir-dist d))
           (if (> m 0.0)
               (dir+ (dir dx 0.0 0.0) (dir-scale d (/ (soft-clamp-component m dsecs) m)))
               dv)]
          [(= ddm (abs ddy))
           (define d (dir dx 0.0 dz))
           (define m (dir-dist d))
           (if (> m 0.0)
               (dir+ (dir 0.0 dy 0.0) (dir-scale d (/ (soft-clamp-component m dsecs) m)))
               dv)]
          [else
           (define d (dir dx dy 0.0))
           (define m (dir-dist d))
           (if (> m 0.0)
               (dir+ (dir 0.0 0.0 dz) (dir-scale d (/ (soft-clamp-component m dsecs) m)))
               dv)]))
  new-dv)

(: near-aligned? (-> Pos Dir Dir Boolean))
(define (near-aligned? v dv ddv)
  (match-define (pos x y z) v)
  (match-define (dir dx dy dz) dv)
  (match-define (dir ddx ddy ddz) ddv)
  (define ddm (max (abs ddx) (abs ddy) (abs ddz)))
  (cond [(= ddm (abs ddx))
         (if (> (abs dy) (abs dz))
             (< (abs (- (round-half z) z)) (* 0.5 offset-margin))
             (< (abs (- (round-half y) y)) (* 0.5 offset-margin)))]
        [(= ddm (abs ddy))
         (if (> (abs dz) (abs dx))
             (< (abs (- (round-half x) x)) (* 0.5 offset-margin))
             (< (abs (- (round-half z) z)) (* 0.5 offset-margin)))]
        [else
         (if (> (abs dx) (abs dy))
             (< (abs (- (round-half y) y)) (* 0.5 offset-margin))
             (< (abs (- (round-half x) x)) (* 0.5 offset-margin)))]))

(: nearest-aligned (-> Pos Dir Dir Pos))
(define (nearest-aligned v dv ddv)
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

(: nearest-aligned* (-> Pos Pos))
(define (nearest-aligned* v)
  (match-define (pos x y z) v)
  (pos (round-half x) (round-half y) (round-half z)))

(: near-axis? (-> Dir Flonum Boolean))
(define (near-axis? d thresh)
  (match-define (dir dx dy dz) d)
  (> (max (abs dx) (abs dy) (abs dz)) thresh))

(: near-axial? (->* [Affine] [Flonum] Boolean))
(define (near-axial? t [thresh 0.999])
  (match-define (affine dx dy dz _) t)
  (and (near-axis? dx thresh) (near-axis? dy thresh) (near-axis? dz thresh)))

(: nearest-axial (-> Affine Affine))
(define (nearest-axial t)
  (match-define (affine dx dy dz p) t)
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
  (assert (affine ax ay az p) values))

(: affine->axis-angle (-> Affine (Values Dir Flonum)))
(define (affine->axis-angle t)
  (match-define (affine dx dy dz _) t)
  (match-define (dir m00 m10 m20) dx)
  (match-define (dir m01 m11 m21) dy)
  (match-define (dir m02 m12 m22) dz)
  (define θ (acos (max -1.0 (min 1.0 (* 0.5 (+ m00 m11 m22 -1.0))))))
  (define 2s (flsqrt (+ (sqr (- m21 m12)) (sqr (- m02 m20)) (sqr (- m10 m01)))))
  (define ax (/ (- m21 m12) 2s))
  (define ay (/ (- m02 m20) 2s))
  (define az (/ (- m10 m01) 2s))
  (if (and (< -inf.0 (min ax ay az))
           (< (max ax ay az) +inf.0))
      (values (dir ax ay az) θ)
      (values +z 0.0)))

(: axialize-angular (-> Affine Dir))
(define (axialize-angular t)
  (cond [(near-axial? t)  zero-dir]
        [else
         (define t0 (nearest-axial t))
         (define-values (axis angle) (affine->axis-angle (affine-compose t (affine-inverse t0))))
         (dir-scale axis (delta->velocity (- angle) (* 2.0 pi)))]))

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

(: find-jump-dir (-> Pict3D Flonum Flonum Pos Dir Dir
                     (Listof (Pair Flonum Dir))
                     (Listof (Pair Flonum Dir))
                     (Values (U #f Dir) (Listof (Pair Flonum Dir)))))
(define (find-jump-dir coll-pict time mag v dv down hits jumps)
  ;(printf "jump: hits = ~v~n" hits)
  
  ;; Consider every normal we hit on the last frame as a jump direction
  (define ns (remove-duplicates (map (inst cdr Flonum Dir) hits)))
  
  ;; Consider the normals of surfaces currently touching that are perpendicular to a hit normal
  (define slide-ns
    (remove-duplicates
     (for*/fold ([slide-ns : (Listof Dir)  empty])
                ([face  (in-list faces)]
                 [face-n  (in-value (dir-negate face))]
                 [n  (in-list ns)]
                 #:when (< (abs (dir-dot face-n n)) 1e-8))
       (define ps (trace-face coll-pict v (dir-scale face 0.625) n))
       (define p (ormap (λ ([p : (U #f Pos)]) p) ps))
       (cond [(not p)  slide-ns]
             [else  (list* n face-n slide-ns)]))
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
  
  (define slide-n
    (dir-normalize
     (for/fold ([slide-n : Dir  zero-dir]) ([n  (in-list slide-ns)])
       (dir+ slide-n n))))
  
  (define slide-dv
    (if slide-n
        (dir-scale slide-n (* slide-jump-multiplier mag))
        zero-dir))
  
  (define bounce-n
    (dir-normalize
     (for/fold ([bounce-n : Dir  zero-dir]) ([n  (in-list bounce-ns)])
       (dir+ bounce-n n))))
  
  (define bounce-dv
    (cond [(not bounce-n)  zero-dir]
          [(<= (dir-dot down bounce-n) 0.0)  (dir-scale down (- mag))]
          [else  zero-dir]))
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
  (define last-jump-time (apply max -inf.0 (map (inst car Flonum Dir) jumps)))
  (cond
    [(< (- time last-jump-time) jump-cooldown-time)  pstate]
    [(and (> (hash-ref keys " " (λ () 0.0)) 0.0)
          (not (empty? hits)))
     (match man
       [(moving dv da #f)
        (define down (let ([down  (dir-normalize ddv)])
                       (if down down zero-dir)))
        (define mag (jump-mag dv ddv))
        (define-values (jump-dir new-jumps) (find-jump-dir coll-pict time mag v dv down hits jumps))
        (cond [(and jump-dir (> (dir-dist jump-dir) 1.0))
               (define new-dv (dir+ dv jump-dir))
               (define axis (let ([axis  (dir-normalize (dir-cross dv ddv))])
                              (if axis axis zero-dir)))
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
        (cond
          [(let ([cooldown-time  (* (- 1.0 (/ (dir-dot dv req-d) (dir-dist dv)))
                                    jump->arrow-cooldown-time)])
             (ormap (λ ([tn : (Pair Flonum Dir)]) (< (- time (car tn)) cooldown-time)) jumps))
           ;; Too soon after a jump
           pstate]
          [else
           ;; Reject every surface hit on the last frame
           (define arrow-dir
             (for/fold ([arrow-dir : Dir  req-d]) ([tn  (in-list hits)])
               (dir-reject arrow-dir (cdr tn))))
           ;; Direction of gravity
           (define down (assert (dir-normalize ddv) values))
           ;; Vertical velocity
           (define dz (assert (dir-project dv down) values))
           ;; Horizontal velocity
           (define dh (dir- dv dz))
           (define speed (dir-dist dh))
           ;; Change direction in the plane orthogonal to gravity, but don't change speed
           (define new-dv (dir+ dz (dir-scale arrow-dir (max user-speed speed))))
           (player-state v ddv t tm (moving new-dv da jumping?))])]
       [_  pstate])]
    [else  pstate]))

;; ---------------------------------------------------------------------------------------------------
;; Edge pivoting

(: axialize/align/trace (-> Pos Affine Pict3D (U #f Pos)))
(define (axialize/align/trace old-v old-t coll-pict)
  (define v (nearest-aligned* old-v))
  (define t (nearest-axial old-t))
  (define delta-v (pos- v old-v))
  (define delta-t (affine-compose t (affine-inverse old-t)))
  (define-values (dist _) (trace/player coll-pict old-v old-t delta-v delta-t))
  (if (< dist 1.0) #f v))

(: player-state-maybe-edge-pivot (-> player-state Pict3D player-state))
(define (player-state-maybe-edge-pivot pstate coll-pict)
  (match pstate
    [(player-state old-v ddv (? (λ (t) (near-axial? t 0.99)) old-t) (timers keys hits jumps)
                   (moving (? (λ (dv) (>= (dir-dist dv) min-pivot-speed)) dv) da (? not jumping?)))
     (define face (argmax (λ ([face : Dir]) (dir-dot dv face)) faces))
     (define center-offset (dir-scale face (- 0.5 offset-margin)))
     (define v (axialize/align/trace old-v old-t coll-pict))
     (cond
       [(not v)  pstate]
       [else
        (define-values (score edge-offset)
          (for/fold ([best-score : Flonum  +inf.0]
                     [best-edge-offset : Dir  zero-dir])
                    ([offsets  (in-list edge-offsets)])
            (define edge-offset (second offsets))
            (cond
              [(or (ormap (λ ([offset : Dir]) (< (dir-dot offset face) 0.1)) offsets)
                   (ormap (λ ([offset : Dir])
                            (define v1 (pos+ (pos+ v center-offset)
                                             (dir-scale (dir- center-offset offset) 0.75)))
                            (define v2 (pos+ v1 (dir-scale face 0.25)))
                            (trace coll-pict v1 v2))
                          offsets))
               (values best-score best-edge-offset)]
              [else
               (define datas
                 (for/fold ([datas : (Listof Surface-Data)  empty]) ([offset  (in-list offsets)])
                   (define v1 (pos+ (pos+ v center-offset)
                                    (dir-scale (dir- offset center-offset) 0.75)))
                   (define v2 (pos+ v1 (dir-scale face 0.25)))
                   (define data (trace/data coll-pict v1 v2))
                   (if (and data (surface-data-normal data)) (cons data datas) datas)))
               (cond
                 [(< (length datas) 3)
                  (values best-score best-edge-offset)]
                 [else
                  (define ns (map surface-data-normal* datas))
                  (define n (dir-normalize (dir+ (dir+ (first ns) (second ns)) (third ns))))
                  (cond
                    [(or (not n) (> (dir-dot face n) -0.999))
                     (values best-score best-edge-offset)]
                    [else
                     (define score (/ (dir-dot dv n) (dir-dist dv)))
                     (if (< score best-score)
                         (values score edge-offset)
                         (values best-score best-edge-offset))])])])))
        
        (define axis (dir-normalize (dir-cross face edge-offset)))
        (cond
          [(and (< score +inf.0) axis)
           (printf "edge pivot~n")
           (define rot-offset (dir-scale face 0.5 #;(+ 0.5 offset-margin)))
           ;; Convert tangential velocity to rotational
           (define speed (/ (dir-dist dv) 0.5))
           (define new-pivot (pivoting rot-offset axis speed (* 1.0 pi) (moving dv zero-dir #f)))
           (define new-timers (timers keys empty empty))
           (define new-v v #;(pos+ v face (- offset-margin)))
           (player-state new-v ddv identity-affine new-timers new-pivot)]
          [else  pstate])])]
    [_  pstate]))

;; ---------------------------------------------------------------------------------------------------
;; Corner pivoting

(: player-state-maybe-corner-pivot (-> player-state Pict3D player-state))
(define (player-state-maybe-corner-pivot pstate coll-pict)
  (match pstate
    [(player-state old-v
                   (? (λ (d) (> (dir-dist d) 0.0)) ddv)
                   (? (λ (t) (near-axial? t 0.9)) old-t)
                   (timers keys hits jumps)
                   (moving (? (λ (dv) (>= (dir-dist dv) min-pivot-speed)) dv) da (? not jumping?)))
     (define face (argmax (λ ([face : Dir]) (dir-dot dv face)) faces))
     (define center-offset (dir-scale face (- 0.5 offset-margin)))
     (define v (axialize/align/trace old-v old-t coll-pict))
     (cond
       [(not v)  pstate]
       [else
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
                    [(> (dir-dot face n) -0.999)
                     (values best-score best-offset)]
                    [else
                     (define score (/ (dir-dot dv n) (dir-dist dv)))
                     (if (< score best-score)
                         (values score offset)
                         (values best-score best-offset))])])])))
        
        (define axis
          (let* ([axis  (dir-cross face offset)]
                 [axis  (dir-project axis ddv)]
                 [axis  (if axis (dir-normalize axis) axis)])
            axis))
        (cond
          [(and (< score +inf.0) axis)
           (printf "corner pivot~n")
           (define rot-offset (dir-scale face 0.5))
           ;; Convert tangential velocity to rotational
           (define speed (/ (dir-dist dv) 0.5))
           (define new-pivot (pivoting rot-offset axis speed (* 1.0 pi) (moving dv zero-dir #f)))
           (define new-timers (timers keys empty empty))
           (define new-v v #;(pos+ v face (- offset-margin)))
           (player-state new-v ddv identity-affine new-timers new-pivot)]
          [else  pstate])])]
    [_  pstate]))

;; ---------------------------------------------------------------------------------------------------
;; Maneuver normalization

(: player-state-normalize (-> player-state Flonum Flonum player-state))
(define (player-state-normalize pstate time dsecs)
  ;(printf "new frame~n")
  (match-define (player-state v ddv t (timers keys hits jumps) man) pstate)
  (define new-jumps (filter (λ ([tn : (Pair Flonum Dir)]) (< (- time (car tn)) 500.0)) jumps))
  ;; Keep only the surfaces hit on this frame
  (define new-hits  (filter (λ ([tn : (Pair Flonum Dir)]) (= time (car tn))) hits))
  (define new-timers (timers keys new-hits new-jumps))
  (define-values (new-v new-t new-man)
    (match man
      [(moving dv da jumping?)
       (define aligned? (near-aligned? v dv ddv))
       (define axial? (near-axial? t))
       (define new-v (if aligned? (nearest-aligned v dv ddv) v))
       (define new-t (if axial? (nearest-axial t) t))
       (define new-dv
         (let* ([dv  (soft-clamp-velocity dv ddv dsecs)]
                [dv  (if (< (dir-dist dv) 0.1) zero-dir dv)]
                [dv  (align-velocity new-v dv ddv dsecs)])
           dv))
       (define new-da
         (cond [axial?  (if (< (dir-dist da) 0.1) zero-dir da)]
               [jumping?  da]
               [else    (axialize-angular new-t)]))
       
       (values new-v new-t (moving new-dv new-da jumping?))]
      [_  (values v t man)]))
  (player-state new-v ddv new-t new-timers new-man))

(: position-offset (-> (Listof Dir) Dir))
;; Offset the position in the direction of every normal to maintain a minimum distance from
;; scene geometry
(define (position-offset ns)
  (for/fold ([add-v : Dir  zero-dir]) ([n  (in-list ns)])
    (dir+ add-v (dir-scale n offset-margin))))

;; ---------------------------------------------------------------------------------------------------
;; Movement

(: normals-opposite? (-> (Listof Dir) Boolean))
(define (normals-opposite? ns)
  (for/or : Boolean ([n0  (in-list ns)])
    (for/or : Boolean ([n1  (in-list ns)])
      (< (dir-dot n0 n1) 0.0))))

(: advance/trace (-> Pict3D Flonum Pos Affine Dir Dir Flonum (Listof Dir) (Values Pos Affine)))
(define (advance/trace coll-pict dsecs v t delta-v da dist ns)
  (define part-delta-v (dir+ (dir-scale delta-v dist) (position-offset ns)))
  (define part-rot-t (angular-velocity->affine (dir-scale da dist) dsecs))
  (define-values (new-dist _) (trace/player coll-pict v t part-delta-v part-rot-t))
  (if (< new-dist 1.0)
      (values v t)
      (values (pos+ v part-delta-v)
              (affine-compose part-rot-t t))))

(: player-state-execute (-> player-state Pict3D Flonum Flonum player-state))
(define (player-state-execute pstate coll-pict time dsecs)
  (match-define (player-state v ddv t (and tm (timers keys hits jumps)) man) pstate)
  ;(printf "dsecs = ~v~n" dsecs)
  (match man
    ;; Moving maneuvers
    [(moving dv da jumping?)
     ;; Change in velocity
     (define delta-v (dir+ (dir-scale dv dsecs) (dir-scale ddv (* 0.5 (sqr dsecs)))))
     ;; Change in rotation
     (define rot-t (angular-velocity->affine da dsecs))
     ;; Surfaces hit and minimum distance
     (define-values (dist datas) (trace/player coll-pict v t delta-v rot-t))
     (define num-hits (length datas))
     ;; The maximum distance a point on the cube could have moved due to rotation
     (define max-corner-dist (* (dir-dist da) player-radius dsecs))
     ;; Extract unique surface normals
     (define ns (remove-duplicates (map surface-data-normal* datas)))
     (define new-hits (remove-duplicates (append (map (λ ([n : Dir]) (cons time n)) ns) hits)))
     #|
If we hit something, we're going to move and rotate the cube for only part of the time for this frame.
If we're not careful, we could end up with the cube intersecting scene geometry.

Here's how to be careful: run physics in subdivided time when the maximum distance a point could have
moved due to rotation is greater than the smallest allowable distance between the cube and scene
geometry.

To maintain the smallest allowable distance between the cube and scene geometry, we need to
 * Run physics in subdivided time when two surfaces are hit whose normals make an acute angle.
 * Move the cube away from surfaces hit in the direction of the surface normals.
|#
     (cond
       #;
       [(and (< dist 1.0) (normals-opposite? (map (inst cdr Flonum Dir) new-hits)))
        (player-state v ddv t tm (moving zero-dir zero-dir #f))]
       [(and (< dist 1.0) (> max-corner-dist offset-margin))
        ;; Execute the maneuver in subdivided time
        (let ([pstate  (player-state-execute pstate coll-pict time (* dsecs 0.5))])
          (player-state-execute pstate coll-pict time (* dsecs 0.5)))]
       [(< dist 1.0)
        ;; React to collisions in partial time
        (define part-dsecs (* dist dsecs))
        ;; Record hits
        (define new-hits (remove-duplicates (append (map (λ ([n : Dir]) (cons time n)) ns) hits)))
        (define new-tm (timers keys new-hits jumps))
        ;; Advance position and rotation in partial time
        (define-values (new-v new-t)
          (advance/trace coll-pict dsecs v t delta-v da dist ns))
        ;; React to every surface (magnitude of rejection determines stop or bounce)
        (define part-dv (dir+ dv (dir-scale ddv part-dsecs)))
        (define add-dv
          (for/fold ([add-dv : Dir  zero-dir])
                    ([data  (in-list datas)])
            ;; Surface normal
            (define n (surface-data-normal* data))
            ;; Magnitude of rejection from the surface
            (define j (* (dir-dot part-dv n) (- -1.0 (restitution-coef ddv n jumping?))))
            ;; Reject this normal a bit
            (dir+ add-dv (dir-scale n (/ j (fl num-hits))))))
        ;; Reject opposing faces that the cube hits on the same frame (otherwise, bouncing between
        ;; walls in a very narrow corridor leads to a velocity explosion)
        (define new-dv
          (for*/fold ([new-dv : Dir  (dir+ part-dv add-dv)]) ([n0  (in-list ns)]
                                                              [tn1  (in-list hits)])
            (match-define (cons t1 n1) tn1)
            (if (and (= t1 time) (< (dir-dot n0 n1) -0.999))
                (dir-reject (dir-reject new-dv n0) n1)
                new-dv)))
        ;; Construct a new maneuver and player state
        (define new-man (moving new-dv zero-dir #f))
        (define new-pstate (player-state new-v ddv new-t new-tm new-man))
        ;; Continue executing the maneuver for the remaining time
        (player-state-execute new-pstate coll-pict time (- dsecs (max (* 0.01 dsecs) part-dsecs)))]
       [else
        ;; No collisions: advance in time normally
        (define new-dv (dir+ dv (dir-scale ddv dsecs)))
        (define new-v (pos+ v delta-v))
        (define new-t (affine-compose rot-t t))
        (define new-man (moving new-dv da jumping?))
        (player-state new-v ddv new-t tm new-man)])]
    ;; Pivoting maneuvers
    [(pivoting offset axis speed deg next-man)
     ;; Find the center of rotation (relative to the cube center) and transformed axis
     (define toffset (transform-dir offset t))
     (define taxis   (transform-dir axis t))
     ;; Determine how much to rotate (min ensures delta-a = deg when finished pivoting)
     (define delta-a (min deg (* speed dsecs)))
     ;; Compute the rotation matrix
     (define rot-t (rotate taxis (radians->degrees delta-a)))
     ;; Compute the new center point
     (define delta-v (dir+ toffset (transform-dir (dir-negate toffset) rot-t)))
     ;; Trace to see if we hit anything during rotation
     (define-values (dist datas) (trace/player coll-pict v t delta-v rot-t))
     ;; Compute angular velocity and maximum distance a point could have moved due to rotation
     (define da (dir-scale taxis speed))
     (define max-corner-dist (* (dir-dist da) player-radius dsecs))
     ;; Extract unique surface normals
     (define ns (remove-duplicates (map surface-data-normal* datas)))
     (cond
       [(and (< dist 1.0)
             (or (> max-corner-dist offset-margin)
                 (normals-opposite? ns)))
        ;; Subdivide physics if we hit something and rotating might put the cube inside the scene
        (let ([pstate  (player-state-execute pstate coll-pict time (* dsecs 0.5))])
          (player-state-execute pstate coll-pict time (* dsecs 0.5)))]
       [(or (< dist 1.0) (= delta-a deg))
        ;; React to collisions
        (define part-dsecs (* dist dsecs))
        ;; Record hits
        (define new-hits (remove-duplicates (append (map (λ ([n : Dir]) (cons time n)) ns) hits)))
        (define new-tm (timers keys new-hits jumps))
        ;; Advance position and rotation in partial time
        (define-values (new-v new-t) (advance/trace coll-pict dsecs v t delta-v da dist ns))
        ;; Derive tangential velocity for center point, use max component as new linear velocity
        (define new-dv (dir-cross da (dir-negate toffset)))
        ;; Construct a *moving* maneuver and a new player state
        (define new-man (moving new-dv zero-dir #f))
        (define new-pstate (player-state new-v ddv new-t new-tm new-man))
        ;; Use `player-state-advance` to give the player a chance to respond between pivots
        (player-state-advance new-pstate coll-pict time (- dsecs part-dsecs))]
       [else
        ;; No collisions: advance in time normally
        (let ([v  (pos+ v delta-v)]
              [t  (affine-compose rot-t t)])
          (define pivot-man (pivoting offset axis speed (- deg delta-a) next-man))
          (player-state v ddv t tm pivot-man))])]))

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
  ;(printf "v = ~v~ndv = ~v~n~n" v dv)
  
  (let* ([pstate  (player-state-maybe-jump pstate coll-pict time dsecs)]
         [pstate  (player-state-maybe-arrow pstate coll-pict time dsecs "left" -dx dv)]
         [pstate  (player-state-maybe-arrow pstate coll-pict time dsecs "right" +dx dv)]
         [pstate  (player-state-maybe-arrow pstate coll-pict time dsecs "up" -dy dv)]
         [pstate  (player-state-maybe-arrow pstate coll-pict time dsecs "down" +dy dv)]
         [pstate  (player-state-normalize pstate time dsecs)]
         [pstate  (player-state-maybe-edge-pivot pstate coll-pict)]
         [pstate  (player-state-maybe-corner-pivot pstate coll-pict)]
         [pstate  (for/fold ([pstate : player-state  pstate]) ([_  (in-range physics-frames)])
                    (player-state-execute pstate coll-pict time (/ dsecs (fl physics-frames))))])
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
