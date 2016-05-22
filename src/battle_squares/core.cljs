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

;; Utils...
(defn angle-for-movement [[x y]]
  (Math/atan2 y x))

(defn on-screen-position [[x y]]
  [(mod x 1500) (mod y 750)])

(defn next-position [[x y] angle distance]
  [(+ x (int (Math/floor (* distance (Math/cos angle))))) (+ y (int (Math/floor (* distance (Math/sin angle)))))])


;; Game Entities...



(defn computer-player []
  {:id       (rand-int 100000)
   :type     :computer
   :position [(rand-int 1501) (rand-int 751)]
   :angle    (angle-for-movement [(rand-int 3) (rand-int 3)])
   :speed    (+ (rand-int 4) 1)
   :health   2500})

(defn player [_]
  {:id       :user
   :position [750 375]
   :type     :player
   :angle    0
   :speed    0
   :health   2500})


;; Game State...

(defn add-player [game player]
  (assoc-in game [:players (:id player)] player))

(defn add-starting-player [game]
  (add-player game (player game)))

(defn add-random-aliens [game number]
  (reduce add-player game (take number (repeatedly computer-player))))


;; Game Management...

(defn start-game-state []
  (-> {:mouse-position [750 375]
       :canvas-size [1500 750]
       :players {}
       :bullets {}}
      (add-starting-player)
      (add-random-aliens 10)))

(defonce app-state (atom (start-game-state)))


;; Render...

(defn draw-line-at-angle [context [x y] angle length]
  (.beginPath context)
  (set! (. context -lineWidth) 4)
  (set! (. context -strokeStyle) "#AA0000")
  (.moveTo context x y)
  (.lineTo context (+ x (* length (Math/cos angle))) (+ y (* length (Math/sin angle))))
  (.stroke context))

(defn draw-bullet [context {position :position}]
  (.beginPath context)
  (set! (.-fillStyle context) "Red")
  (.arc context (first position) (second position) 4 0 (* Math/PI 2) false)
  (.fill context))

(defn draw-bullets [context bullets]
  (doseq [bullet bullets]
    (draw-bullet context bullet)))

(defn draw-turret [context position angle length]
  (draw-line-at-angle context position angle length))

;(def colour-health {100 "ff"
;                   90 "ef"
;                   80 "df"
;                   70 "cf"
;                   60 "bf"
;                   50 "af"
;                   40 "8f"
;                   30 "6f"
;                   20 "4f"
;                   10 "2f"
;                    0 "00 "})

(defn player-color [player]
  (if (= :computer (:type player)) "#00FF00" "#0000FF"))
    ;(str "#00FF00" (colour-health (:health player)) "00")
    ;(str "#0000" (colour-health (:health player)))))

(defn player-size [player]
  (Math/sqrt (:health player)))

(defn draw-player [context {position :position angle :angle :as player}]
  (let [player-size       (player-size player)]
    (println player-size)
    (.save context)
    ;; draw body...
    (.translate context (first position) (second position))
    (.rotate context angle)
    (set! (.-fillStyle context) (player-color player))
    (.fillRect context (quot player-size -2) (quot player-size -2) player-size player-size)
    (.restore context)

    ;; draw turret...
    (draw-turret context position angle (* 0.7 player-size))
  ))

(defn draw-players [context players]
  (doseq [player players] (draw-player context player)))

(defn render []
  (let [game    @app-state
        target  (.getElementById js/document "myCanvas")
        context (.getContext target "2d")
        players (vals (:players game))
        bullets (vals (:bullets game))]
    (.clearRect context 0 0 1500 750)
    (draw-players context players)
    (draw-bullets context bullets)
    (animation-frame render)))


;; Game Tick...

(defn hit-check [player bullet]
  (let [[x y]             (:position player)
        size              (player-size player)
        [other-x other-y] (:position bullet)]
    (when (and (not (= (:owner bullet) (:id player)))
               (> other-x (- x size))
               (< other-x (+ x size))
               (> other-y (- y size))
               (< other-y (+ y size)))
      bullet)))

(defn hit-by-bullet [player bullets]
  (when bullets
    ;(println position bullets)
    (first (filter identity
      (map (partial hit-check player) bullets)))))

(defn bullet [player]
  {:id       (rand-int 100000)
   :position (:position player)
   :angle    (:angle player)
   :life     0
   :speed    15
   :owner    (:id player)})

(defn speed-for-movement [x y x2 y2]
  (if (> (Math/sqrt (+ (* (- x x2)(- x x2)) (* (- y y2) (- y y2)))) 40) 7 0))

(defn update-player [game player]
  (assoc-in game [:players (:id player)] player))

(defn remove-player [game player]
  (update-in game [:players] dissoc (:id player)))

(defn update-user-movement [game]
  (let [[mouse-x mouse-y]   (:mouse-position game)
        player              (get-in game [:players :user])
        [player-x player-y] (:position player)]
    (if (:dead player)
      (update-player game (assoc player :speed 0))
      (update-player game (assoc player :angle (angle-for-movement [(- mouse-x player-x) (- mouse-y player-y)])
                                        :speed (min (speed-for-movement mouse-x mouse-y player-x player-y) 20))))))

(defn move-user-player [game]
  (let [{:keys [position angle speed dead dead-time] :as player} (get-in game [:players :user])]
    (cond
      (not dead) (update-player game (assoc player :position (on-screen-position (next-position position angle speed))))
      dead       (update-player game (assoc player :dead-time (+ 1 dead-time))))))

(defn tick-user-player [game]
  (-> (update-user-movement game)
      (move-user-player)))

(defn bullets [game]
  (vals (:bullets game)))

(defn player-shoot [game id]
  (let [player  (get-in game [:players id])
        bullet  (bullet player)]
    (assoc-in game [:bullets (:id bullet)] bullet)))

(defn maybe-shoot [game player-id]
  (if (> (rand-int 1000) 995)
    (player-shoot game player-id)
    game))

(defn tick-computer-player [game {:keys [position angle speed dead dead-time] :as computer-player}]
    (cond
      (not dead)             (-> (update-player game
                                                (assoc computer-player
                                                  :position (on-screen-position (next-position position angle speed))))
                                 (maybe-shoot (:id computer-player)))
      (and dead
           (<= dead-time 30)) (update-player game (assoc computer-player :dead true
                                                                         :dead-time (+ 1 dead-time)
                                                                         :speed 0))
      (and dead
           (> dead-time 30)) (remove-player game computer-player)))

(defn computer-players [game]
  (filter (fn [player] (= :computer (:type player))) (vals (:players game))))

(defn tick-computer-players [game]
  (reduce tick-computer-player game (computer-players game)))

(defn update-bullet [game bullet]
  (assoc-in game [:bullets (:id bullet)] bullet))

(defn remove-bullet [game bullet]
  (update-in game [:bullets] dissoc (:id bullet)))

(defn tick-bullet [game {:keys [position angle life] :as bullet}]
  (if (< life 30)
    (update-bullet game
                   (assoc bullet :position (on-screen-position (next-position position angle 20))
                                 :life     (+ life 1)))
    (remove-bullet game bullet)))

(defn tick-bullets [game]
  (reduce tick-bullet game (vals (:bullets game))))

(defn players [game]
  (vals (:players game)))

(defn damage-from-bullet [game player bullet]
  (let [health (max (- (:health player) 250) 0)
        dead   (= health 0)]
    (update-player game (assoc player :health health
                                      :dead   dead))))

(defn reward-health-for-hit [game bullet]
  (let [owner (get-in game [:players (:owner bullet)])]
    (if (and owner (not (:dead owner)))
      (update-player game (assoc owner :health (+ 250 (:health owner))))
      game)))

(defn update-player-hit [game player bullet]
  (-> (damage-from-bullet game player bullet)
      (reward-health-for-hit bullet)
      (remove-bullet bullet)))

(defn collision-detect [game player]
  (if-let [bullet (hit-by-bullet player (bullets game))]
    (update-player-hit game player bullet)
    game))

(defn tick-collision-detect [game]
  (reduce collision-detect game (players game)))

(defn user-player-is-dead [game]
  (and (get-in game [:players :user :dead])
       (> 120 (get-in game [:players :user :dead-time]))))

(defn no-more-opponents [game]
  (not (first (filter identity (computer-players game)))))

(defn tick-end-game? [game]
  (if (or (user-player-is-dead game)
          (no-more-opponents game))
    (assoc game :restart true)
    game))

(defn tick-game [game]
  (if (:restart game)
    (start-game-state)
    (-> (tick-user-player game)
        (tick-computer-players)
        (tick-bullets)
        (tick-collision-detect)
        (tick-end-game?)
        )))

(defn game-ticker []
  (swap! app-state tick-game)
  (js/setTimeout game-ticker 17))

(defn update-mouse-position [current-state mouse-x mouse-y]
  (assoc current-state :mouse-position [mouse-x mouse-y]))

(defn handle-mouse-move [mouse-event]
  (let [mouse-x (aget mouse-event "clientX")
        mouse-y (aget mouse-event "clientY")]
    (swap! app-state update-mouse-position mouse-x mouse-y)))

(defn user-shoot [game]
  (player-shoot game :user))

(defn handle-mouse-click [_]
  (swap! app-state user-shoot))

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
    (game-ticker)
    ))

(comment start-game)

;; TODO
;; Player Get Hit By Bullets
;; Have Hit Points - start with 100, do 10 damage.
;; When you hit, receive the hitpoints.
;; Rotate ships to draw