#lang typed/racket

(require pict3d
         math/flonum)

(provide (all-defined-out))

#;
(provide humanoid-rig
         stand
         run
         walk)

;; ===================================================================================================

(struct Inner-Group-Transform ([path : (Listof Tag)]
                               [transform : Inner-Transform])
  #:transparent)

(struct Inner-Compose-Transform ([snd : Inner-Transform]
                                 [fst : Inner-Transform])
  #:transparent)

(: compose-inner2 (-> Inner-Transform Inner-Transform Inner-Transform))
(define (compose-inner2 t1 t0)
  (cond [(equal? t1 identity-affine)  t0]
        [(equal? t0 identity-affine)  t1]
        [(and (affine? t1) (affine? t0))  (affine-compose t1 t0)]
        [else  (Inner-Compose-Transform t1 t0)]))

(: compose-inner (-> Inner-Transform * Inner-Transform))
(define (compose-inner . ts)
  (if (empty? ts)
      identity-affine
      (let loop ([t0  (first ts)] [ts  (rest ts)])
        (if (empty? ts)
            t0
            (compose-inner2 t0 (loop (first ts) (rest ts)))))))

(: group-inner (-> (Listof Tag) Inner-Transform * Inner-Transform))
(define (group-inner n . ts)
  (let ([t  (apply compose-inner ts)])
    (cond [(equal? t identity-affine)  identity-affine]
          [(empty? n)  t]
          [else  (Inner-Group-Transform n t)])))

(define-type Inner-Transform (U Affine Inner-Group-Transform Inner-Compose-Transform))

(: transform-inside (-> Pict3D Inner-Transform Pict3D))
(define (transform-inside p t)
  (match t
    [(Inner-Group-Transform n t0)  (replace-group p n (λ (p) (transform-inside p t0)))]
    [(Inner-Compose-Transform t1 t0)  (transform-inside (transform-inside p t0) t1)]
    [(? affine? t)  (transform p t)]))

(: transform-position-delta (-> Pict3D (Listof Tag) Inner-Transform (U #f Dir)))
(define (transform-position-delta rig path t)
  ;; Get old locations
  (match-define t0s (map-group/transform rig path (λ ([t : Affine] _) t)))
  (define n (length t0s))
  (cond
    [(= n 0)  #f]
    [else
     (define v0s (map (λ ([t : Affine]) (transform-pos origin t)) t0s))
     ;; Get new locations
     (define t1s (map-group/transform (transform-inside rig t) path (λ ([t : Affine] _) t)))
     (define v1s (map (λ ([t : Affine]) (transform-pos origin t)) t1s))
     ;; Compute average difference between ankle positions
     (define ds (map pos- v0s v1s))
     (for/fold ([delta : Dir  zero-dir]) ([d  (in-list ds)])
       (dir+ delta (dir-scale d (/ n))))]))

;; ===================================================================================================

(: expt/sgn (-> Real Real Real))
(define (expt/sgn x k)
  (define s (sgn x))
  (* s (expt (abs x) k)))

(: mix (-> Real Real Real Real))
(define (mix x y a)
  (+ (* x (- 1 a)) (* y a)))

(: make-cycle (-> Real (->* [Real Real Real] [Real] Real)))
(define ((make-cycle time) offset mn mx [k 1])
  (+ mn (* (- mx mn) (* 1/2 (+ 1 (expt/sgn (sin (* 2 pi (+ time offset))) k))))))

;; ===================================================================================================
;; Humanoid rig

(: arm-rig (->* [] [#:upper Nonnegative-Real #:lower Nonnegative-Real] Pict3D))
(define (arm-rig #:upper [u #i5/16]
                 #:lower [l #i5/16])
  (let ([u  (fl u)] [l  (fl l)])
    (define wrist (basis 'wrist identity-affine))
    (define elbow (group (combine (move-z wrist (- l))
                                  (move-z (basis 'lower-arm (scale l)) (* -0.5 l)))
                         'elbow))
    (group (combine (move-z elbow (- u))
                    (move-z (basis 'upper-arm (scale u)) (* -0.5 u)))
           'shoulder)))

(: leg-rig (->* [] [#:upper Nonnegative-Real #:lower Nonnegative-Real] Pict3D))
(define (leg-rig #:upper [u #i1/2]
                 #:lower [l #i1/2])
  (let ([u  (fl u)] [l  (fl l)])
    (define ankle (basis 'ankle identity-affine))
    (define knee (group (combine (move-z ankle (- l))
                                 (move-z (basis 'lower-leg (scale l)) (* -0.5 l)))
                         'knee))
    (group (combine (move-z knee (- u))
                    (move-z (basis 'upper-leg (scale u)) (* -0.5 u)))
           'hip)))

(: upper-torso-rig (->* []
                        [#:neck
                         Nonnegative-Real
                         #:shoulders Nonnegative-Real
                         #:chest Nonnegative-Real]
                        Pict3D))
(define (upper-torso-rig #:neck [n #i1/8]
                         #:shoulders [s #i1/2]
                         #:chest [c #i5/16])
  (let ([n  (fl n)] [s  (fl s)] [c  (fl c)])
    (define atlas (basis 'atlas identity-affine))
    (define left-shoulder  (basis 'left-shoulder  identity-affine))
    (define right-shoulder (basis 'right-shoulder identity-affine))
    (define t1
      (group (combine
              ;; Joints
              (move-z atlas n)
              (move-y left-shoulder (* 0.5 s))
              (scale-y (move-y right-shoulder (* 0.5 s)) -1.0)
              ;; Bones
              (move-z (basis 'neck (scale n)) (* 0.5 n))
              (basis 'collar (scale s)))
             't1))
    (group (combine (move-z t1 c)
                    (move-z (basis 'chest (scale c)) (* 0.5 c)))
           't10)))

(: lower-torso-rig (->* []
                        [#:abdomen
                         Nonnegative-Real
                         #:hips Nonnegative-Real]
                        Pict3D))
(define (lower-torso-rig #:abdomen [a #i5/16]
                         #:hips [h #i3/8])
  (let ([a  (fl a)] [h  (fl h)])
    (define left-hip  (basis 'left-hip  identity-affine))
    (define right-hip (basis 'right-hip identity-affine))
    (define sacrum
      (group (combine
              ;; Joints
              (move-y left-hip (* 0.5 h))
              (scale-y (move-y right-hip (* 0.5 h)) -1.0)
              ;; Bones
              (basis 'pelvis (scale h)))
             'sacrum))
    (group (combine (move-z sacrum (- a))
                    (move-z (basis 'abdomen (scale a)) (* -0.5 a)))
           't11)))

(: humanoid-rig
   (->* []
        [#:neck Nonnegative-Real
         #:chest Nonnegative-Real
         #:abdomen Nonnegative-Real
         #:hips Nonnegative-Real
         #:upper-leg Nonnegative-Real
         #:lower-leg Nonnegative-Real
         #:shoulders Nonnegative-Real
         #:upper-arm Nonnegative-Real
         #:lower-arm Nonnegative-Real
         #:head Nonnegative-Real
         #:hand Nonnegative-Real
         #:foot Nonnegative-Real]
        Pict3D))
(define (humanoid-rig #:neck [neck #i1/8]
                      #:chest [chest #i5/16]
                      #:abdomen [abdomen #i5/16]
                      #:hips [hips #i1/4]
                      #:upper-leg [upper-leg #i1/2]
                      #:lower-leg [lower-leg #i1/2]
                      #:shoulders [shoulders #i1/2]
                      #:upper-arm [upper-arm #i5/16]
                      #:lower-arm [lower-arm #i5/16]
                      #:head [head #i1/4]
                      #:hand [hand #i1/4]
                      #:foot [foot #i5/16])
  (define arm-pict (arm-rig #:upper upper-arm #:lower lower-arm))
  (define leg-pict (leg-rig #:upper upper-leg #:lower lower-leg))
  (let* ([upper-torso-pict  (upper-torso-rig #:neck neck #:shoulders shoulders #:chest chest)]
         [lower-torso-pict  (lower-torso-rig #:abdomen abdomen #:hips hips)]
         [upper-torso-pict  (pin upper-torso-pict '(left-shoulder)  arm-pict '(shoulder))]
         [upper-torso-pict  (pin upper-torso-pict '(right-shoulder) arm-pict '(shoulder))]
         [lower-torso-pict  (pin lower-torso-pict '(left-hip)  leg-pict '(hip))]
         [lower-torso-pict  (pin lower-torso-pict '(right-hip) leg-pict '(hip))]
         [pict  (basis 'root identity-affine)]
         [pict  (pin pict '(root) upper-torso-pict)]
         [pict  (pin pict '(root) lower-torso-pict)]
         [head  (fl head)]
         [hand  (fl hand)]
         [foot  (fl foot)]
         [pict  (pin pict '(wrist) (move-z (basis 'hand (scale hand)) (* -0.5 hand)))]
         [pict  (pin pict '(ankle) (move-x (basis 'foot (scale foot)) (* 0.5 foot)))]
         [pict  (pin pict '(atlas) (move-z (basis 'head (scale head)) (* 0.5 head)))])
    (move-z pict (+ lower-leg upper-leg abdomen))))

;; ===================================================================================================
;; Animation transforms

;; ---------------------------------------------------------------------------------------------------
;; Standing transforms

(: stand-upper (->* [] [#:crouch Real #:spread Real] Inner-Transform))
(define (stand-upper #:crouch [crouch 10] #:spread [spread 10])
  ;; Arms
  (define wrist-t (group-inner '(wrist) (rotate-z spread)))
  (define elbow-t (group-inner '(elbow) (rotate-x (* -1 spread)) (rotate-y (* -1 crouch)) wrist-t))
  (define ls-t (group-inner '(left-shoulder)  (rotate-x (* 1 spread)) elbow-t))
  (define rs-t (group-inner '(right-shoulder) (rotate-x (* 1 spread)) elbow-t))
  ;; Upper torso
  (define atlas-t (group-inner '(atlas) (rotate-y (* -1/3 crouch))))
  (define t1-t (group-inner '(t1) (rotate-y (* -1/3 crouch)) atlas-t ls-t rs-t))
  (group-inner '(t10) (rotate-y crouch) t1-t))

(: stand-lower (->* [] [#:crouch Real #:spread Real] Inner-Transform))
(define (stand-lower #:crouch [crouch 10] #:spread [spread 10])
  ;; Legs
  (define ankle-t
    (group-inner '(ankle) (rotate-y (* (- (+ 1/3 -2/3 -2/3 3/2)) crouch)) (rotate-z spread)))
  (define knee-t (group-inner '(knee) (rotate-x (- spread)) (rotate-y (* 3/2 crouch)) ankle-t))
  (define ll-t (group-inner '(left-hip)  (rotate-y (* -2/3 crouch)) (rotate-x spread) knee-t))
  (define rl-t (group-inner '(right-hip) (rotate-y (* -2/3 crouch)) (rotate-x spread) knee-t))
  ;; Lower torso
  (define sacrum-t (group-inner '(sacrum) (rotate-y (* -2/3 crouch)) ll-t rl-t))
  (define t11-t (group-inner '(t11) (rotate-y (* 1/3 crouch)) sacrum-t))
  (group-inner '(root) t11-t))

(: stand-lower* (->* [Pict3D] [#:crouch Real #:spread Real] Inner-Transform))
(define (stand-lower* rig #:crouch [crouch 10] #:spread [spread 10])
  (define t (stand-lower #:crouch crouch #:spread spread))
  (define d1 (transform-position-delta rig '(root t11 sacrum  left-hip knee ankle) t))
  (define d2 (transform-position-delta rig '(root t11 sacrum right-hip knee ankle) t))
  (define d (if (and d1 d2) (dir-scale (dir+ d1 d2) 0.5) (or d1 d2)))
  (if d (group-inner '(root) (move d) t) t))

(: stand (->* [Pict3D] [#:crouch Real #:spread Real] Inner-Transform))
(define (stand rig #:crouch [crouch 10] #:spread [spread 10])
  (compose-inner (stand-upper #:crouch crouch #:spread spread)
                 (stand-lower* rig #:crouch crouch #:spread spread)))

;; ---------------------------------------------------------------------------------------------------
;; Running transforms

(: run-upper (->* [Real] [#:freq Real #:lean Real #:sprint Real] Inner-Transform))
(define (run-upper time #:freq [freq 1] #:lean [lean 20] #:sprint [sprint 1/2])
  (define angle (make-cycle (* freq time)))
  ;; Hips and shoulders swivel angle
  (define h (angle 0 (mix -10 -30 sprint) (mix 10 30 sprint) 3/4))
  ;; Upper angles
  (define ls (+ (* -1/2 lean) (angle 0 (mix 30 60 sprint) (mix 0 -30 sprint))))
  (define rs (+ (* -1/2 lean) (angle 0 (mix 0 -30 sprint) (mix 30 60 sprint))))
  (define le (angle 4/32 (mix -60 -15 sprint) (mix -90 -150 sprint) 2/3))
  (define re (angle 4/32 (mix -90 -150 sprint) (mix -60 -15 sprint) 2/3))
  ;; Left arm
  (define le-t (group-inner '(elbow) (rotate-y le)))
  (define ls-t (group-inner '(left-shoulder)  (rotate-z (* +3/4 h)) (rotate-y ls) le-t))
  ;; Right arm
  (define re-t (group-inner '(elbow) (rotate-y re)))
  (define rs-t (group-inner '(right-shoulder) (rotate-z (* -3/4 h)) (rotate-y rs) re-t))
  ;; Upper torso
  (define atlas-t (group-inner '(atlas) (rotate-z (* 5/6 h))))
  (group-inner '(t10) (rotate-z (- h)) atlas-t ls-t rs-t))

(: run-lower (->* [Real] [#:freq Real #:lean Real #:sprint Real] Inner-Transform))
(define (run-lower time #:freq [freq 1] #:lean [lean 20] #:sprint [sprint 1/2])
  (define angle (make-cycle (* freq time)))
  ;; Hips and shoulders swivel angle
  (define h (angle 0 (mix -10 -30 sprint) (mix 10 30 sprint) 3/4))
  ;; Lower angles
  (define ll (angle 0 (mix -60 -90 sprint) 30))
  (define rl (angle 0 30 (mix -60 -90 sprint)))
  (define lk (angle 5/32 (mix 105 150 sprint) 0 3/4))
  (define rk (angle 5/32 0 (mix 105 150 sprint) 3/4))
  (define lf (angle 9/32 60 -15 1/2))
  (define rf (angle 9/32 -15 60 1/2))
  ;; Left leg
  (define lf-t (group-inner '(ankle) (rotate-y (max lf (- (+ ll lk))))))
  (define lk-t (group-inner '(knee) (rotate-y lk) lf-t))
  (define ll-t (group-inner '(left-hip) (rotate-z (* -1 h)) (rotate-y ll) lk-t))
  ;; Right leg
  (define rf-t (group-inner '(ankle) (rotate-y (max rf (- (+ rl rk))))))
  (define rk-t (group-inner '(knee) (rotate-y rk) rf-t))
  (define rl-t (group-inner '(right-hip) (rotate-z (* +1 h)) (rotate-y rl) rk-t))
  ;; Lower torso
  (define sacrum-t (group-inner '(sacrum) (rotate-y (* -1 lean)) ll-t rl-t))
  (define t11-t (group-inner '(t11) (rotate-z h) (rotate-y (* 1/2 lean)) sacrum-t))
  (group-inner '(root) (rotate-y (* 1/2 lean)) t11-t))

(: run-lower* (->* [Pict3D Real] [#:freq Real #:lean Real #:sprint Real] Inner-Transform))
(define (run-lower* rig time #:freq [freq 1] #:lean [lean 20] #:sprint [sprint 1/2])
  (define t (run-lower time #:freq freq #:lean lean #:sprint sprint))
  ;; Interpolate bounce amount from jog to sprint
  (define angle2 (make-cycle (* freq 2 time)))
  (define b (mix (angle2 0 -3/64 1/64) 0 sprint))
  ;; Adjust for leaning to put feet on the floor
  (define d (transform-position-delta rig '(root t11 sacrum) t))
  (define a (if d (dir-dz d) 0))
  (group-inner '(root) (move-z (+ a b)) t))

(: run (->* [Pict3D Real] [#:freq Real #:lean Real #:sprint Real] Inner-Transform))
(define (run rig time #:freq [freq 1] #:lean [lean 20] #:sprint [sprint 1/2])
  (compose-inner (run-upper time #:freq freq #:lean lean #:sprint sprint)
                 (run-lower* rig time #:freq freq #:lean lean #:sprint sprint)))

;; ---------------------------------------------------------------------------------------------------
;; Walking transforms

(: walk-upper (->* [Real] [#:freq Real #:lean Real #:swing Real] Inner-Transform))
(define (walk-upper time #:freq [freq 1/2] #:lean [lean 10] #:swing [swing 1/4])
  (define angle (make-cycle (* freq time)))
  ;; Hips and shoulders swivel angle
  (define h (angle 0 (mix 0 -45 swing) (mix 0 45 swing) 3/4))
  ;; Upper angles
  (define ls (+ (* -1/2 lean) (angle 0 (mix 10 90 swing) (mix 10 -90 swing))))
  (define rs (+ (* -1/2 lean) (angle 0 (mix 10 -90 swing) (mix 10 90 swing))))
  (define le (angle 8/32 (mix -15 -90 swing) (mix -15 0 swing) 3/4))
  (define re (angle 8/32 (mix -15 0 swing) (mix -15 -90 swing) 3/4))
  ;; Left arm
  (define lh-t (group-inner '(wrist) (rotate-y (angle 8/32 (mix 0 -45 swing) 0))))
  (define le-t (group-inner '(elbow) (rotate-y le) lh-t))
  (define ls-t (group-inner '(left-shoulder) (rotate-z (* +3/4 h)) (rotate-y ls) le-t))
  ;; Right arm
  (define rh-t (group-inner '(wrist) (rotate-y (angle 8/32 0 (mix 0 -45 swing)))))
  (define re-t (group-inner '(elbow) (rotate-y re) rh-t))
  (define rs-t (group-inner '(right-shoulder) (rotate-z (* -3/4 h)) (rotate-y rs) re-t))
  ;; Upper torso
  (define atlas-t (group-inner '(atlas) (rotate-z (* 5/6 h))))
  (group-inner '(t1) (rotate-z (- h)) atlas-t ls-t rs-t))

(: walk-lower (->* [Real] [#:freq Real #:lean Real #:swing Real] Inner-Transform))
(define (walk-lower time #:freq [freq 1/2] #:lean [lean 10] #:swing [swing 1/4])
  (define angle (make-cycle (* freq time)))
  ;; Hips and shoulders swivel angle
  (define h (angle 0 (mix 0 -45 swing) (mix 0 45 swing) 3/4))
  ;; Lower angles
  (define ll (angle 0 (mix -30 -60 swing) 30))
  (define rl (angle 0 30 (mix -30 -60 swing)))
  (define lk (angle 5/32 (mix 45 90 swing) 0 3/4))
  (define rk (angle 5/32 0 (mix 45 90 swing) 3/4))
  ;; Left leg
  (define lf-t (group-inner '(ankle) (rotate-y (max (- (+ ll lk)) (angle 13/32 15 -15 2/3)))))
  (define lk-t (group-inner '(knee) (rotate-y lk) lf-t))
  (define ll-t (group-inner '(left-hip) (rotate-z (* -1 h)) (rotate-y ll) lk-t))
  ;; Right leg
  (define rf-t (group-inner '(ankle) (rotate-y (max (- (+ rl rk)) (angle 13/32 -15 15 2/3)))))
  (define rk-t (group-inner '(knee) (rotate-y rk) rf-t))
  (define rl-t (group-inner '(right-hip) (rotate-z (* +1 h)) (rotate-y rl) rk-t))
  ;; Lower torso
  (define sacrum-t  (group-inner '(sacrum) (rotate-y (* -1 lean)) ll-t rl-t))
  (define t11-t (group-inner '(t11) (rotate-z h) (rotate-y (* 1/2 lean)) sacrum-t))
  (group-inner '(root) (rotate-y (* 1/2 lean)) t11-t))

(: walk-lower* (->* [Pict3D Real] [#:freq Real #:lean Real #:swing Real] Inner-Transform))
(define (walk-lower* rig time #:freq [freq 1/2] #:lean [lean 10] #:swing [swing 1/4])
  (define t (walk-lower time #:freq freq #:lean lean #:swing swing))
  ;; Bounce
  (define angle2 (make-cycle (* 2 freq time)))
  (define b (angle2 4/32 (mix -1/32 -1/16 swing) (mix 1/32 0 swing) 3/4))
  ;; Adjust for leaning to put feet on the floor
  (define d (transform-position-delta rig '(t11 sacrum) t))
  (define a (if d (dir-dz d) 0))
  (group-inner '(root) (move-z (+ a b)) t))

(: walk (->* [Pict3D Real] [#:freq Real #:lean Real #:swing Real] Inner-Transform))
(define (walk rig time #:freq [freq 1/2] #:lean [lean 10] #:swing [swing 1/4])
  (compose-inner (walk-upper time #:freq freq #:lean lean #:swing swing)
                 (walk-lower* rig time #:freq freq #:lean lean #:swing swing)))
