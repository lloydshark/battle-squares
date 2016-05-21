(ns battle-squares.core
  (:require
   [goog.dom :as dom]
   [goog.events :as events]))

(enable-console-print!)
;(def log (.-log js/console))

(def animation-frame
  (or (.-requestAnimationFrame js/window)
      (.-webkitRequestAnimationFrame js/window)
      (.-mozRequestAnimationFrame js/window)
      (.-oRequestAnimationFrame js/window)
      (.-msRequestAnimationFrame js/window)
      (fn [callback] (js/setTimeout callback 17))))

(defn angle-for-movement [[x y]]
  (Math/atan2 y x))

(defn on-screen-position [[x y]]
  [(mod x 1500) (mod y 750)])

(defn random-alien []
  {:position [(rand-int 1501) (rand-int 751)]
   :angle    (angle-for-movement [(rand-int 3) (rand-int 3)])
   :speed    (rand-int 4)
   :health   100})

(defn starting-game-state []
  {:player {:position [750 375]
            :angle    0
            :speed    0
            :health   100}
   :bullets (list)
   :mouse-position [750 375]
   :aliens (take 10 (repeatedly random-alien))})


;; Starting Game State...
(defonce app-state (atom (starting-game-state)))

(def player-size 50)

(defn next-position [[x y] angle distance]
  [(+ x (int (Math/floor (* distance (Math/cos angle))))) (+ y (int (Math/floor (* distance (Math/sin angle)))))])

(defn draw-line-at-angle [context [x y] angle length]
  (.beginPath context)
  (set! (. context -lineWidth) 4)
  (set! (. context -strokeStyle) "#AA0000")
  (.moveTo context x y)
  (.lineTo context (+ x (* length (Math/cos angle))) (+ y (* length (Math/sin angle))))
  (.stroke context))

(defn draw-turret [context position angle]
  (draw-line-at-angle context position angle 35))

(defn draw-player [context {position :position angle :angle hit :hit}]
  (set! (.-fillStyle context) (if hit "Red" "LightBlue"))
  (.fillRect context (- (first position) 25) (- (second position) 25) player-size player-size)
  ;(set! (.-fillStyle context) "Red")
  ;(.fillRect context (- (first position) 2) (- (second position) 35) 4 35)
  (draw-turret context position angle)
  )

(defn draw-bullet [context {position :position}]
  (.beginPath context)
  (set! (.-fillStyle context) "Red")
  (.arc context (first position) (second position) 4 0 (* Math/PI 2) false)
  (.fill context))

(defn draw-bullets [context bullets]
  (doseq [bullet bullets]
    (draw-bullet context bullet)))

(def colour-health {100 "#00ff00"
                   90 "#00ef00"
                   80 "#00df00"
                   70 "#00cf00"
                   60 "#00bf00"
                   50 "#00af00"
                   40 "#008f00"
                   30 "#006f00"
                   20 "#004f00"
                   10 "#002f00"
                    0 "#000000"})

(defn draw-alien [context {position :position angle :angle hit :hit health :health}]
  (set! (.-fillStyle context) (if hit "Red" (or (get colour-health health) "Green")))
  (.fillRect context (- (first position) 25) (- (second position) 25) player-size player-size)
  (draw-turret context position angle))

(defn draw-aliens [context aliens]
  (doseq [alien aliens]
    (draw-alien context alien)))

(defn render []
  (let [current-state @app-state
        target  (.getElementById js/document "myCanvas")
        context (.getContext target "2d")
        player  (:player current-state)
        bullets (:bullets current-state)
        aliens  (:aliens current-state)]
    ;(println current-state)
    (.clearRect context 0 0 1500 750)
    (draw-player context player)
    (draw-bullets context bullets)
    (draw-aliens context aliens)
    (animation-frame render)
    ))


;(defn movement [x y]
;    [x y])

;(defn more-left [game amount]
;  (let [current-movement (get-in game [:player :movement])]
;    (assoc-in game [:player :movement]
;      (movement (max (* -1 (or amount 1)) -5) (second current-movement)))))
;
;(defn more-right [game amount]
;  (let [current-movement (get-in game [:player :movement])]
;    (assoc-in game [:player :movement]
;      (movement (min (or amount 1) 5) (second current-movement)))))
;
;(defn more-up [game amount]
;  (let [current-movement (get-in game [:player :movement])]
;    (assoc-in game [:player :movement]
;      (movement (first current-movement) (max (* -1 (or amount 1)) -5)))))
;
;(defn more-down [game amount]
;  (let [current-movement (get-in game [:player :movement])]
;    (assoc-in game [:player :movement]
;      (movement (first current-movement) (min (or amount 1) 5)))))

;(defn move-player [game]
;  (let [{:keys [position movement]} (:player game)]
;    (assoc-in game [:player :position]
;      [(mod (+ (first position) (first movement)) 1500) (mod (+ (second position) (second movement)) 750)])))

;(defn stop-x [game]
;  (let [current-movement (get-in game [:player :movement])]
;    (assoc-in game [:player :movement]
;      (movement 0 (second current-movement)))))
;
;(defn stop-y [game]
;  (let [current-movement (get-in game [:player :movement])]
;    (assoc-in game [:player :movement]
;      (movement (first current-movement) 0))))

;(defn player-stop-y-if-needed [game mouse-y player-y]
;  (if (= mouse-y player-y) (stop-y game) game))
;
;(defn player-more-down-if-needed [game mouse-y player-y]
;  (if (> mouse-y player-y) (more-down game (+ 1 (quot (- mouse-y player-y) 20))) game))
;
;(defn player-more-up-if-needed [game mouse-y player-y]
;  (if (< mouse-y player-y) (more-up game (+ 1 (quot (- player-y mouse-y) 20))) game))
;
;(defn player-stop-x-if-needed [game mouse-x player-x]
;  (if (= mouse-x player-x) (stop-x game) game))
;
;(defn player-more-right-if-needed [game mouse-x player-x]
;  (if (> mouse-x player-x) (more-right game (+ 1 (quot (- mouse-x player-x) 20))) game))
;
;(defn player-more-left-if-needed [game mouse-x player-x]
;  (if (< mouse-x player-x) (more-left game (+ 1 (quot (- player-x mouse-x) 20))) game))

;(defn update-player-movement [game]
;  (let [[player-x player-y] (get-in game [:player :position])
;        [mouse-x mouse-y]   (get game :mouse-position)]
;    (-> (player-more-left-if-needed game mouse-x player-x)
;        (player-more-right-if-needed mouse-x player-x)
;        (player-stop-x-if-needed mouse-x player-x)
;        (player-more-up-if-needed mouse-y player-y)
;        (player-more-down-if-needed mouse-y player-y)
;        (player-stop-y-if-needed mouse-y player-y))))


(defn update-bullet [{:keys [position angle life]}]
  (when (< life 30)
    {:position (on-screen-position (next-position position angle 20))
     :angle angle
     :life (+ life 1)}))

(defn update-bullets [bullets]
  (filter identity
    (map update-bullet bullets)))

(defn update-bullets-game-state [game]

  (assoc game :bullets (update-bullets (:bullets game)))
  )

(defn hit-check [[x y] size bullet]
  (let [[other-x other-y] (:position bullet)]
    (and (> other-x (- x size))
         (< other-x (+ x size))
         (> other-y (- y size))
         (< other-y (+ y size)))))

(defn hit-by-bullet [position bullets]
  (when bullets
    ;(println position bullets)
    (first (filter identity
      (map (partial hit-check position 25) bullets)))))

(defn update-alien [bullets {:keys [position angle speed health] :as alien}]
  ;(println alien bullets)
  (let [hit       (hit-by-bullet position bullets)
        health    (if hit (- health 10) health)
        dead      (<= health 0)
        dead-time (or (:dead-time alien) 0)]
    (when (< dead-time 30)
        {:position  (if dead
                      position
                      (on-screen-position (next-position position angle speed)))
         :health    health
         :dead      dead
         :dead-time (if dead (+ dead-time 1) dead-time)
         :angle     angle
         :speed     (if dead 0 speed)}
         )))

(defn bullet [thing]
  {:position (on-screen-position (next-position (:position thing) (:angle thing) 40))
   :angle    (:angle thing)
   :life     0
   :speed    15})

(defn aliens-shoot [current-state]
  (let [aliens (:aliens current-state)]
    (reduce (fn [current-state alien]
              (if (> (rand-int 1000) 995)
                (assoc current-state :bullets
                                     (conj (:bullets current-state) (bullet alien)))
                current-state)) current-state aliens)))

(defn update-aliens-game-state [game]
  (let [aliens (:aliens game)]
    (-> (assoc game :aliens (filter identity (map (partial update-alien (:bullets game)) aliens)))
        (aliens-shoot))))

(defn speed-for-movement [x y x2 y2]
  (if (and (= x x2) (= y y2))
    0
    (+ 1 (quot (Math/sqrt (+ (* (- x x2)(- x x2)) (* (- y y2) (- y y2)))) 20))))

(defn update-player-movement [game]
  (let [[mouse-x mouse-y]   (:mouse-position game)
        player              (:player game)
        [player-x player-y] (:position player)]
    (assoc game :player (assoc player :angle (angle-for-movement [(- mouse-x player-x) (- mouse-y player-y)])
                                      :speed (min (speed-for-movement mouse-x mouse-y player-x player-y) 20)))))

(defn move-player [game]
  (let [{:keys [position angle speed]} (:player game)]
    (assoc-in game [:player :position]
              (on-screen-position (next-position position angle speed)))))

(defn update-player [game]
  (let [{:keys [position health angle speed] :as player} (:player game)
        hit       (hit-by-bullet position (:bullets game))
        health    (if hit (- health 10) health)
        dead      (<= health 0)
        dead-time (or (:dead-time player) 0)]
    (if (< dead-time 120)
      (assoc game :player
                  {:position  (if dead
                                position
                                (on-screen-position (next-position position angle speed)))
                   :health    health
                   :dead      dead
                   :dead-time (if dead (+ dead-time 1) dead-time)
                   :angle     angle
                   :speed     (if dead 0 speed)})
      (assoc game :restart true))))

(defn update-player-in-game [game]
  (-> (update-player-movement game)
      (update-player)))

(defn update-game [game]
  (-> (update-player-in-game game)
      (update-bullets-game-state)
      ;(update-player-movement)
      (update-aliens-game-state)
      ;(move-player)
      ))

(defn tick []
  (when (:restart @app-state)
    (reset! app-state (starting-game-state)))
  (swap! app-state update-game)
  (js/setTimeout tick 17))

;(defn handle-input [character]
 ; (case character
  ;  87 (more-up) ;(move-player-up)
   ; 65 (more-left) ;(move-player-left)
;    83 (more-down) ;(move-player-down)
 ;   68 (more-right) ;(move-player-right)
  ;  :do-nothing))

(defn update-mouse-position [current-state mouse-x mouse-y]
  (assoc current-state :mouse-position [mouse-x mouse-y]))

(defn handle-mouse-move [mouse-event]
  (let [mouse-x (aget mouse-event "clientX")
        mouse-y (aget mouse-event "clientY")]
    (swap! app-state update-mouse-position mouse-x mouse-y)))


(defn player-shoot [current-state]
  (let [player  (:player current-state)
        bullets (:bullets current-state)]
    (assoc current-state :bullets (conj bullets (bullet player)))))

(defn handle-mouse-click [_]
    (swap! app-state player-shoot))

;; Use wrapper functions so that we can hot-reload updated handlers...
(defn attach-handle-mouse-move [event] (handle-mouse-move event))
(defn attach-handle-mouse-click [event] (handle-mouse-click event))
(defn attach-render [] (render))

(defn event-setup []
  (events/listen (dom/getWindow)
      (.-MOUSEMOVE events/EventType) #(attach-handle-mouse-move %))
  (events/listen (dom/getWindow)
      (.-CLICK events/EventType) #(attach-handle-mouse-click %))
  ;(events/listen (dom/getWindow)
  ;   (.-KEYDOWN events/EventType) #(handle-input (aget % "keyCode")))
  )

;; One time setup...
(defonce start-game
  (do
    (event-setup)
    (animation-frame attach-render)
    (tick)))

(comment start-game)

;; TODO
;; Player Get Hit By Bullets
;; Have Hit Points - start with 100, do 10 damage.
;; When you hit, receive the hitpoints.
;; Rotate ships to draw