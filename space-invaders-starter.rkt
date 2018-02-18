;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; =================================================================================================
;; =================================================================================================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-HEIGHT (image-height TANK))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))



(define MISSILE (ellipse 5 15 "solid" "red"))

(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))



;; =================================================================================================
;; =================================================================================================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition



#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

(define GX (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                      (list (make-missile 150 300) (make-missile 100 200))
                      (make-tank 50 1)))
(define G4 (make-game (list (make-invader 150 100 1.5) (make-invader 100 100 -1.5))
                      (list (make-missile 150 300) (make-missile 100 200))
                      (make-tank 50 1)))

(define LOM1 (list M1 M2 (make-missile 100 200)))
(define LOI1 (list I1 (make-invader 100 100 -10)))


;; =================================================================================================
;; =================================================================================================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                       ; Game
    (on-tick   update-game)         ; Game -> Game
    (to-draw   render)              ; Game -> Image
    (stop-when hit-floor? render)   ; Game -> Boolean
    (on-key    handle-key)))        ; Game KeyEvent -> Game




;; =================================================================================================
;; ON-TICK Functions:

;; Game -> Game
;; produce the next tank, next invader(s), next missile(s)
(check-random (update-game (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                                      (list (make-missile 150 300) (make-missile 100 200))
                                      (make-tank 50 1)))
              (if (< (random INVADE-RATE) 5)
                  (make-game (list (make-invader 162 112 12) (make-invader 88 112 -12) (make-invader (random WIDTH) -10 INVADER-X-SPEED))
                             (list (make-missile 150 290) (make-missile 100 190))
                             (make-tank 52 1))
                  (make-game (list (make-invader 162 112 12) (make-invader 88 112 -12))
                             (list (make-missile 150 290) (make-missile 100 190))
                             (make-tank 52 1))))

;(define (update-game s) s) ;stub

(define (update-game s)
  (make-game (update-invaders (update-loi (game-missiles s) (game-invaders s)))
             (update-missiles (update-lom (game-missiles s) (game-invaders s)))
             (update-tank (game-tank s))))



;; ListOfMissile ListOfInvader -> ListOfMissile
;; remove the missile that hits an invader (from the list)
(check-expect (update-lom LOM1 LOI1) (list M1 (make-missile 100 200)))

;(define (update-lom lom loi) lom) ;stub

(define (update-lom lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (hits? (first lom) loi)
             (update-lom (rest lom) loi)
             (cons (first lom) (update-lom (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; produce true if the missile hits an invader
(check-expect (hits? M1 LOI1) false)
(check-expect (hits? M2 LOI1) true)

;(define (hits? m loi) false) ;stub

(define (hits? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (<= (- (invader-x (first loi)) HIT-RANGE) (missile-x m) (+ (invader-x (first loi)) HIT-RANGE))
                  (<= (- (invader-y (first loi)) HIT-RANGE) (missile-y m) (+ (invader-y (first loi)) HIT-RANGE)))
             true
             (hits? m (rest loi)))]))


;; ListOfMissile ListOfInvader -> ListOfInvader
;; remove the invader that is hit by a missile (from the list)
(check-expect (update-loi LOM1 LOI1) (list (make-invader 100 100 -10)))

;(define (update-loi lom loi) loi) ;stub

(define (update-loi lom loi)
  (cond [(empty? loi) empty]
        [else
         (if (is-hit? lom (first loi))
             (update-loi lom (rest loi))
             (cons (first loi) (update-loi lom (rest loi))))]))

;; ListOfMissile Invader -> Boolean
;; produce true if the invader is hit by missile
(check-expect (is-hit? LOM1 (make-invader 100 100 -10)) false)
(check-expect (is-hit? LOM1 I1) true)

;(define (is-hit? lom i) false) ;stub

(define (is-hit? lom i)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (- (invader-x i) HIT-RANGE) (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
                  (<= (- (invader-y i) HIT-RANGE) (missile-y (first lom)) (+ (invader-y i) HIT-RANGE)))
             true
             (is-hit? (rest lom) i))]))



;; ListOfInvader -> ListOfInvader
;; produce the updated list of invaders
(check-random (update-invaders empty) (if (< (random INVADE-RATE) 5)
                                          (cons (make-invader (random WIDTH) -10 INVADER-X-SPEED) empty)
                                          empty))
(check-random (update-invaders (list (make-invader 150 100 12) (make-invader 100 100 -12)))
              (if (< (random INVADE-RATE) 5)
                  (list  (make-invader 162 112 12) (make-invader 88 112 -12) (make-invader (random WIDTH) -10 INVADER-X-SPEED))
                  (list (make-invader 162 112 12) (make-invader 88 112 -12))))

;(define (update-invaders loi) loi) ;stub
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define (update-invaders loi)
  (cond [(empty? loi) (if (< (random INVADE-RATE) 5)
                          (cons (make-invader (random WIDTH) -10 INVADER-X-SPEED) empty)
                          empty)]
        [else
         (cons (update-invader (first loi))
               (update-invaders (rest loi)))]))

;; Invader -> Invader
;; produce the updated invader
;; x increases/decreases if dx is positive/negative respectively
;; y always increases
;; when an invader hits the wall it reflects from it
(check-expect (update-invader (make-invader 150 100 12)) (make-invader 162 112 12))
(check-expect (update-invader (make-invader 100 100 -12)) (make-invader 88 112 -12))
(check-expect (update-invader (make-invader INVADER-WIDTH/2 100 -12)) (make-invader INVADER-WIDTH/2 100 12))
(check-expect (update-invader (make-invader (- WIDTH INVADER-WIDTH/2) 100 12)) (make-invader (- WIDTH INVADER-WIDTH/2) 100 -12))

;(define (update-invader i) i) ;stub

(define (update-invader invader)
  (if (or (<= (+ (invader-x invader) (invader-dx invader)) INVADER-WIDTH/2) (>= (+ (invader-x invader) (invader-dx invader)) (- WIDTH INVADER-WIDTH/2)))
      (make-invader (invader-x invader)
                    (invader-y invader)
                    (- (invader-dx invader)))
      (make-invader (+ (invader-x invader) (invader-dx invader))
                    (+ (invader-y invader) (abs (invader-dx invader)))
                    (invader-dx invader))))


;; ListOfMissile -> ListOfMissile
;; produce the updated list of missiles
;; delete the missiles that fly out of the screen
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles (list (make-missile 150 300) (make-missile 100 200)))
              (list (make-missile 150 290) (make-missile 100 190)))
(check-expect (update-missiles (list (make-missile 150 300) (make-missile 100 (- MISSILE-HEIGHT/2))))
              (list (make-missile 150 290)))

;(define (update-missiles lom) lom) ;stub
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (on-screen? (first lom))
             (cons (update-missile (first lom))
                   (update-missiles (rest lom)))
             (update-missiles (rest lom)))]))


;; Missile -> Boolean
;; produce true if the missile is visible on the screen
(check-expect (on-screen? (make-missile 100 200)) true)
(check-expect (on-screen? (make-missile 150 (- MISSILE-HEIGHT/2))) false)

;(define (on-screen? m) false) ;stub

(define (on-screen? m)
  (> (missile-y m) (- MISSILE-HEIGHT/2)))


;; Missile -> Missile
;; produce the updated position of missile
(check-expect (update-missile (make-missile 150 300)) (make-missile 150 290))
(check-expect (update-missile (make-missile 100 200)) (make-missile 100 190))

;(define (update-missile m) m) ;stub

(define (update-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; produce the updated position of the tank
;; if the tank hits the wall it changes direction
(check-expect (update-tank (make-tank 50 1)) (make-tank 52 1))
(check-expect (update-tank (make-tank 50 -1)) (make-tank 48 -1))
(check-expect (update-tank (make-tank  TANK-WIDTH/2 -1)) (make-tank TANK-WIDTH/2 1))
(check-expect (update-tank (make-tank (- WIDTH TANK-WIDTH/2) 1)) (make-tank (- WIDTH TANK-WIDTH/2) -1))

;(define (update-tank t) t) ;stub

(define (update-tank t)
  (if (or (<= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) TANK-WIDTH/2) (>= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (- WIDTH TANK-WIDTH/2)))
      (make-tank (tank-x t) (- (tank-dir t)))
      (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))))




;; =================================================================================================
;; TO-DRAW Functions:

;; We have to draw invaders, missiles and a tank
;; Basically I use one helper to iterate over the list of invaders
;; then, when I finish, I use a reference to another helper which
;; iterates over the list of missiles. When I finish, I use a reference
;; to the helper that draws the tank

;; Game -> Image
;; render the game with all missiles, invaders and tank
(check-expect (render (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                                 (list (make-missile 150 300) (make-missile 100 200))
                                 (make-tank 50 1)))
              (place-image
               INVADER
               150
               100
               (place-image
                INVADER
                100
                100
                (place-image
                 MISSILE
                 150
                 300
                 (place-image
                  MISSILE
                  100
                  200
                  (place-image
                   TANK
                   50
                   (- HEIGHT TANK-HEIGHT/2)
                   BACKGROUND))))))

;(define (render s) empty-image) ;stub

(define (render s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s) BACKGROUND))))

;; ListOfInvader Image -> Image
;; render list of invader on a background image
(check-expect (render-invaders (game-invaders GX) BACKGROUND)
              (place-image
               INVADER
               150
               100
               (place-image
                INVADER
                100
                100
                BACKGROUND)))

;(define (render-invaders loi b) empty-image) ;stub

(define (render-invaders loi b)
  (cond [(empty? loi) b]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) b))]))

;; ListOfMissile Image -> Image
;; render list of missiles on a background image
(check-expect (render-missiles (game-missiles GX) BACKGROUND)
              (place-image
               MISSILE
               150
               300
               (place-image
                MISSILE
                100
                200
                BACKGROUND)))

;(define (render-missiles lom b) empty-image) ;stub

(define (render-missiles lom b)
  (cond [(empty? lom) b]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) b))]))

;; Tank Image -> Image
;; render tank on a background image
(check-expect (render-tank (game-tank GX) BACKGROUND)
              (place-image
               TANK
               50
               (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))

;(define (render-tank t b) empty-image) ;stub

(define (render-tank t b)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               b))



;; =================================================================================================
;; ON-KEY Functions:

;; Game KeyEvent -> Game
;; handle keys
(check-expect (handle-key (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                                     (list (make-missile 150 300) (make-missile 100 200))
                                     (make-tank 50 1)) "right")
              (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                         (list (make-missile 150 300) (make-missile 100 200))
                         (make-tank 50 1)))
(check-expect (handle-key (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                                     (list (make-missile 150 300) (make-missile 100 200))
                                     (make-tank 50 -1)) "right")
              (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                         (list (make-missile 150 300) (make-missile 100 200))
                         (make-tank 50 1)))
(check-expect (handle-key (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                                     (list (make-missile 150 300) (make-missile 100 200))
                                     (make-tank 50 1)) "left")
              (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                         (list (make-missile 150 300) (make-missile 100 200))
                         (make-tank 50 -1)))
(check-expect (handle-key (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                                     (list (make-missile 150 300) (make-missile 100 200))
                                     (make-tank 50 -1)) "left")
              (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                         (list (make-missile 150 300) (make-missile 100 200))
                         (make-tank 50 -1)))
(check-expect (handle-key (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                                     (list (make-missile 150 300) (make-missile 100 200))
                                     (make-tank 50 1)) " ")
              (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                         (list (make-missile 50 (- HEIGHT TANK-HEIGHT)) (make-missile 150 300) (make-missile 100 200))
                         (make-tank 50 1)))
(check-expect (handle-key GX "a") GX)

              
;(define (handle-key s ke) s) ;stub

(define (handle-key s ke)
  (cond [(key=? ke "left") (make-game (game-invaders s) (game-missiles s) (make-tank (tank-x (game-tank s)) -1))]
        [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (make-tank (tank-x (game-tank s)) 1))]
        [(key=? ke " ") (make-game (game-invaders s) (cons (make-missile (tank-x (game-tank s)) (- HEIGHT TANK-HEIGHT)) (game-missiles s)) (game-tank s))]
        [else s]))



;; =================================================================================================
;; STOP-WHEN Functions:

;; Game -> Boolean
;; produce true if any invader in the game hits the floor
(check-expect (hit-floor? (make-game (list (make-invader 150 100 12) (make-invader 100 100 -12))
                                     (list (make-missile 150 300) (make-missile 100 200))
                                     (make-tank 50 1))) false)
(check-expect (hit-floor? (make-game (list (make-invader 150 100 12) (make-invader 100 HEIGHT -12))
                                     (list (make-missile 150 300) (make-missile 100 200))
                                     (make-tank 50 1))) true)

;(define (hit-floor? s) false) ;stub

(define (hit-floor? s)
  (hit-floor-inv? (game-invaders s)))


;; ListOfInvader -> Boolean
;; produce true if any invader hits the floor
(check-expect (hit-floor-inv? (list (make-invader 150 100 12) (make-invader 100 100 -12))) false)
(check-expect (hit-floor-inv? (list (make-invader 150 100 12) (make-invader 100 HEIGHT -12))) true)

;(define (hit-floor-inv? loi) false) ;stub

(define (hit-floor-inv? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (hit-floor-inv? (rest loi)))]))