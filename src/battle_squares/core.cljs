(ns battle-squares.core
  (:require
    [battle-squares.dom :refer [element-by-id]]
    [battle-squares.geometry :as geometry]
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

(defn valid-game-position [game [x y]]
  (let [[game-dimension-x game-dimension-y] (:game-dimensions game)]
    [(mod x game-dimension-x) (mod y game-dimension-y)]))

(defn next-position [[x y] angle distance]
  [(+ x (int (Math/floor (* distance (Math/cos angle)))))
   (+ y (int (Math/floor (* distance (Math/sin angle)))))])

(defn screen-dimensions []
  [(aget (.getElementById js/document "app") "clientWidth")
   (aget (.getElementById js/document "app") "clientHeight")])


;; Game Entities...

(defn computer-player [game]
  (let [[x y] (:game-dimensions game)]
    {:id       (rand-int 100000)
     :type     :computer
     :position [(rand-int x) (rand-int y)]
     :angle    (angle-for-movement [(rand-int 3) (rand-int 3)])
     :speed    (+ (rand-int 4) 1)
     :health   2500}))

(defn player [game]
  (let [[x y] (:game-dimensions game)]
    {:id       :user
     :position [(quot x 2) (quot y 2)]
     :type     :player
     :angle    0
     :speed    0
     :health   2500}))


;; Game State...

(defn add-player [game player]
  (assoc-in game [:players (:id player)] player))

(defn add-starting-player [game]
  (add-player game (player game)))

(defn add-random-aliens [game number]
  (reduce add-player game (take number (repeatedly (partial computer-player game)))))


;; Game Management...

(def game-dimensions [2500 1500])

(defn start-game-state []
  (let [[screen-dimension-x screen-dimension-y] (screen-dimensions)]
    (-> {:game-dimensions   game-dimensions
         :screen-dimensions [screen-dimension-x screen-dimension-y]
         :mouse-position    [(quot screen-dimension-x 2) (quot screen-dimension-y 2)]
         :players {}
         :bullets {}
         :brake   :off}
        (add-starting-player)
        (add-random-aliens 5)
        )))

(defonce app-state (atom (start-game-state)))


;; Render...

(defn draw-line-at-angle [context [x y] angle length width]
  (.beginPath context)
  (set! (. context -lineWidth) width)
  (set! (. context -strokeStyle) "#AA0000")
  (.moveTo context x y)
  (.lineTo context (+ x (* length (Math/cos angle))) (+ y (* length (Math/sin angle))))
  (.stroke context))

(defn draw-line [context [from-x from-y] [to-x to-y]]
  (.beginPath context)
  (set! (. context -lineWidth) 1)
  (set! (. context -strokeStyle) "Silver")
  (.moveTo context from-x from-y)
  (.lineTo context to-x to-y)
  (.stroke context))

(defn area-contains [area position]
  (and (>= (first position) (first area))
       (>= (second position) (second area))
       (<= (first position) (nth area 2))
       (<= (second position) (nth area 3))))

(defn draw-bullet [context {position :position size :size} visible-area]
  (when (area-contains visible-area position)
      (.beginPath context)
      (set! (.-fillStyle context) "Red")
      (.arc context
            (- (first position) (first visible-area))
            (- (second position) (second visible-area)) size 0 (* Math/PI 2) false)
      (.fill context)))

(defn draw-bullets [context bullets visible-area]
  (doseq [bullet bullets]
    (draw-bullet context bullet visible-area)))

(defn draw-turret [context position angle length width]
  (draw-line-at-angle context position angle length width))

(defn player-color [player]
  (if (= :computer (:type player)) "#00FF00" "#0000FF"))

(defn player-size [player]
  (Math/sqrt (:health player)))

(defn draw-player [context {position :position angle :angle :as player} visible-area]
  (when (area-contains visible-area position)
    (let [player-size  (player-size player)
          turret-width (quot player-size 8)]
      (.save context)
      ;; draw body...
      (.translate context
                  (- (first position) (first visible-area))
                  (- (second position) (second visible-area)))
      (.rotate context angle)
      (set! (.-fillStyle context) (player-color player))
      (.fillRect context (quot player-size -2) (quot player-size -2) player-size player-size)
      (.restore context)

      ;; draw turret...
      (draw-turret context [(- (first position)  (first visible-area))
                            (- (second position) (second visible-area))] angle (* 0.7 player-size) turret-width))
  ))

(defn draw-players [context players visible-area]
  (doseq [player players] (draw-player context player visible-area)))

(defn calculate-visible-area [game]
  (let [[player-x player-y] (get-in game [:players :user :position])
        half-screen-x       (quot (first (:screen-dimensions game)) 2)
        half-screen-y       (quot (second (:screen-dimensions game)) 2)]
    [(- player-x half-screen-x) (- player-y half-screen-y)
     (+ player-x half-screen-x) (+ player-y half-screen-y)]))

(defn rect-intersection [[left top right bottom] [left-2 top-2 right-2 bottom-2]]
  (cond
    (< bottom top-2) nil
    (> top bottom-2) nil
    (> left right-2) nil
    (< right left-2) nil
    :default [(max left left-2) (max top top-2) (min right right-2) (min bottom bottom-2)]))

(defn draw-rect [context [left top right bottom]]
  (set! (.-fillStyle context) "white")
  (.fillRect context left top (- right left) (- bottom top)))

(defn screen-coordinates [[left top right bottom] visible-area]
  (let [visible-left (first visible-area)
        visible-top  (second visible-area)]
    [(- left visible-left) (- top visible-top) (- right visible-left) (- bottom visible-top)]))

(defn draw-background [context [game-width game-height] visible-area]
  (draw-rect context
             (screen-coordinates (rect-intersection visible-area [0 0 game-width game-height]) visible-area)))

(defn calculate-visible-terrain-lines [[game-width game-height] visible-area]
  (let [[left top right bottom] (rect-intersection visible-area [0 0 game-width game-height])
        visible-horizontals     (filter #(zero? (rem % 250)) (range top bottom))
        visible-verticals       (filter #(zero? (rem % 250)) (range left right))]
    (concat
      (for [x visible-verticals] {:from {:x x :y top} :to {:x x :y bottom}})
      (for [y visible-horizontals] {:from {:x left :y y} :to {:x right :y y}}))))

(defn screen-coordinate [{:keys [x y]} visible-area]
  (let [visible-left (first visible-area)
        visible-top  (second visible-area)]
    [(- x visible-left) (- y visible-top)]))

(defn draw-terrain [context game visible-area]
  (doseq [line (calculate-visible-terrain-lines (:game-dimensions game) visible-area)]
    (draw-line context
               (screen-coordinate (:from line) visible-area)
               (screen-coordinate (:to line) visible-area))))

(defn render []
  (let [game    @app-state
        game-dimensions (:game-dimensions game)
        [screen-dimension-x screen-dimension-y] (:screen-dimensions game)
        target  (.getElementById js/document "myCanvas")
        context (.getContext target "2d")
        visible-area (calculate-visible-area game)
        players (vals (:players game))
        bullets (vals (:bullets game))]
    (.clearRect context 0 0 screen-dimension-x screen-dimension-y)
    (draw-background context game-dimensions visible-area)
    (draw-terrain context game visible-area)
    (draw-players context players visible-area)
    (draw-bullets context bullets visible-area)
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
   :size     (quot (player-size player) 12)
   :owner    (:id player)})

(defn speed-for-movement [x y x2 y2]
  (if (> (Math/sqrt (+ (* (- x x2)(- x x2)) (* (- y y2) (- y y2)))) 40) 7 0))

(defn update-player [game player]
  (assoc-in game [:players (:id player)] player))

(defn remove-player [game player]
  (update-in game [:players] dissoc (:id player)))

(defn update-user-movement [game]
  (let [visible-area              (calculate-visible-area game)
        [raw-mouse-x raw-mouse-y] (:mouse-position game)
        [mouse-x mouse-y]         [(+ raw-mouse-x (first visible-area))
                                   (+ raw-mouse-y (second visible-area))]
        player              (get-in game [:players :user])
        [player-x player-y] (:position player)]
    (if (:dead player)
      (update-player game (assoc player :speed 0))
      (update-player game (assoc player :angle (angle-for-movement [(- mouse-x player-x) (- mouse-y player-y)])
                                        :speed (if (= (:brake game) :on)
                                                 0
                                                 (min (speed-for-movement mouse-x mouse-y player-x player-y) 20)))))))

(defn collision-test [player new-position other-player]
  (let [[player-x player-y]             new-position
        the-player-size                 (player-size player)
        [other-player-x other-player-y] (:position other-player)
        other-player-size               (player-size other-player)
        collision?                      (and (not= (:id player) (:id other-player))
                                             (>= (+ the-player-size other-player-size)
                                                 (+ (js/Math.abs (- player-x other-player-x))
                                                    (js/Math.abs (- player-y other-player-y)))))]
    ;(println "collision between" player other-player)
    collision?))

(defn collision? [game player-id new-position]
  (let [player (get-in game [:players player-id])]
    (first (filter identity (map (partial collision-test player new-position) (vals (:players game)))))))

(defn prevent-collision [game player-id position new-position]
  (if (collision? game player-id new-position) position new-position))

(defn move-user-player [game]
  (let [{:keys [position angle speed dead dead-time] :as player} (get-in game [:players :user])]
    (cond
      (not dead) (update-player game (assoc player :position (prevent-collision game :user position
                                                               (valid-game-position game (next-position position angle speed)))))
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

(defn update-computer-player [game computer-player-id]
  (if-let [computer-player (get-in game [:players computer-player-id])]
    (let [dead      (:dead computer-player)
          dead-time (:dead-time computer-player)
          position  (:position computer-player)
          angle     (:angle computer-player)
          speed     (:speed computer-player)]
      (cond
        (not dead)             (-> (update-player game
                                                  (assoc computer-player
                                                    :position (prevent-collision game computer-player-id position
                                                                                 (valid-game-position game (next-position position angle speed)))))
                                   (maybe-shoot (:id computer-player)))
        (and dead
             (<= dead-time 30)) (update-player game (assoc computer-player :dead true
                                                                           :dead-time (+ 1 dead-time)
                                                                           :speed 0))
        (and dead
             (> dead-time 30)) (-> (remove-player game computer-player)
                                   (add-player (computer-player game)))))
    game))

(defn computer-chase-player [game computer-player-id]
  (if-let [computer-player        (get-in game [:players computer-player-id])]
    (let [[computer-x computer-y] (:position computer-player)
          user-player             (get-in game [:players :user])
          [player-x player-y]     (:position user-player)]
      (update-player game (assoc computer-player
                            :angle (angle-for-movement [(- player-x computer-x) (- player-y computer-y)]))))
    game))

(defn computer-player-think [game computer-player-id]
  (if (> (rand-int 5000) 4500)
    (computer-chase-player game computer-player-id)
    game))

(defn tick-computer-player [game computer-player-id]
  (-> (computer-player-think game computer-player-id)
      (update-computer-player computer-player-id)))

(defn computer-players [game]
  (map :id (filter (fn [player] (= :computer (:type player))) (vals (:players game)))))

(defn tick-computer-players [game]
  (reduce tick-computer-player game (computer-players game)))

(defn update-bullet [game bullet]
  (assoc-in game [:bullets (:id bullet)] bullet))

(defn remove-bullet [game bullet]
  (update-in game [:bullets] dissoc (:id bullet)))

(defn tick-bullet [game {:keys [position angle life] :as bullet}]
  (if (< life 30)
    (update-bullet game
                   (assoc bullet :position (valid-game-position game (next-position position angle 20))
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

(defn update-mouse-position [game mouse-x mouse-y]
  (assoc game :mouse-position [mouse-x mouse-y]))

(defn handle-mouse-move [mouse-event]
  (let [mouse-x (aget mouse-event "clientX")
        mouse-y (aget mouse-event "clientY")]
    (swap! app-state update-mouse-position mouse-x mouse-y)))

(defn user-shoot [game]
  (player-shoot game :user))

(defn handle-mouse-click [_]
  (swap! app-state user-shoot))

(defn toggle-break [game]
  (assoc game :brake ((:brake game) {:on :off :off :on})))

(defn handle-keydown [keycode]
  (when (= 32 keycode)
    (swap! app-state toggle-break)))

;; Use wrapper functions so that we can hot-reload updated handlers...
(defn attach-handle-mouse-move [event] (handle-mouse-move event))
(defn attach-handle-mouse-click [event] (handle-mouse-click event))
(defn attach-handle-keydown [keycode] (handle-keydown keycode))
(defn attach-render [game-state]
  (render @game-state)
  (attach-render game-state))

;; Event Setup...

(defn event-setup []
  (events/listen (dom/getWindow)
      (.-MOUSEMOVE events/EventType) #(attach-handle-mouse-move %))
  (events/listen (dom/getWindow)
      (.-CLICK events/EventType) #(attach-handle-mouse-click %))
  (events/listen (dom/getWindow)
     (.-KEYDOWN events/EventType) #(attach-handle-keydown (aget % "keyCode")))
  )

;; Canvas Setup...

(defn canvas-setup
  "Ensure that the canvas is set to match the size of its container (typically meaning full screen)..."
  [game-state canvas-element-id canvas-container-element-id]
  (let [game-canvas-holder (element-by-id canvas-container-element-id)
        game-canvas        (element-by-id canvas-element-id)
        screen-dimensions  (geometry/rectangle
                             (geometry/position 0 0)
                             (geometry/position (aget game-canvas-holder "clientWidth")
                                                (aget game-canvas-holder "clientWidth")))]
    (set! (.-width game-canvas) (geometry/width screen-dimensions))
    (set! (.-height game-canvas) (geometry/height screen-dimensions))
    (swap! game-state assoc :screen-dimensions screen-dimensions)))

;; Rendering Setup...

(defn renderer-setup [game-state]
  (animation-frame (fn [] (attach-render game-state))))

;; One time setup...

(def canvas-element-id "gameCanvas")
(def canvas-container-element-id "gameCanvasContainer")

(def game-state (atom {}))

(defonce start-game
  (do
    (canvas-setup game-state canvas-element-id canvas-container-element-id)
    (event-setup)
    (animation-frame attach-render)
    (game-ticker)
    ))

(comment start-game)

;; TODO
;; Bug: Background doesn't draw correctly.
;; Show Edge of game.
;; Prevent Ships from going into each other.
;; Shoot from end of turret.
;; Get slower when bigger.
;; Make aliens smarter.