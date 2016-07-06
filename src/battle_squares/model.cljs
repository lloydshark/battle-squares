(ns battle-squares.model
  (:require [battle-squares.geometry :as geometry]))

;; Accessors

(defn position [player]
  (:position player))

(defn health [player]
  (:health player))

(defn players [game]
  (vals (:players game)))

(defn bullets [game]
  (vals (:bullets game)))

(defn update-canvas-area [game canvas-area]
  (assoc game :canvas-area canvas-area))

(defn toggle-brake [game]
  (assoc game :brake ((:brake game) {:on :off :off :on})))

(defn damage [bullet]
  (* (:size bullet) 60))

(defn update-mouse-position [game mouse-x mouse-y]
  (assoc game :mouse-position [mouse-x mouse-y]))

(defn computer-players [game]
  (map :id (filter (fn [player] (= :computer (:type player))) (vals (:players game)))))

(defn user-player-is-dead [game]
  (and (get-in game [:players :user :dead])
       (> 120 (get-in game [:players :user :dead-time]))))

(defn no-more-opponents [game]
  (not (first (filter identity (computer-players game)))))

(defn game-over? [game]
  (or (user-player-is-dead game)
      (no-more-opponents game)))

(defn calculate-visible-area [game]
  (let [[player-x player-y] (get-in game [:players :user :position])
        half-screen-x       (quot (geometry/width (:canvas-area game)) 2)
        half-screen-y       (quot (geometry/height (:canvas-area game)) 2)]
    [(- player-x half-screen-x) (- player-y half-screen-y)
     (+ player-x half-screen-x) (+ player-y half-screen-y)]))

(defn add-player [game player]
  (assoc-in game [:players (:id player)] player))

(defn get-player [game player-id]
  (get-in game [:players player-id]))

(defn player-size [player]
  (Math/sqrt (:health player)))

(defn alive? [player]
  (pos? (:health player)))

(defn collision-damage [player]
  (quot (health player) 200))

(defn increase-player-health [game player-id health-increase]
  (if-let [player (get-player game player-id)]
    (update-in game [:players player-id] assoc :health (+ (:health player)
                                                          health-increase))
    game))

(defn decrease-player-health [game player-id health-decrease]
  (if-let [player (get-player game player-id)]
    (let [updated-health (max (- (:health player)
                                 health-decrease)
                              0)]
      (update-in game [:players player-id] assoc
                 :health updated-health
                 :dead (= updated-health 0)))
      game))

(defn bullet [player]
  {:id       (rand-int 100000)
   :position (:position player)
   :angle    (:angle player)
   :life     0
   :speed    15
   :size     (quot (player-size player) 12)
   :owner    (:id player)})

(defn owner-of [bullet]
  (:owner bullet))

(defn player-shoot [game id]
  (let [player  (get-in game [:players id])
        bullet  (bullet player)]
    (assoc-in game [:bullets (:id bullet)] bullet)))

(defn user-shoot [game]
  (player-shoot game :user))

(defn game-area [game]
  (:game-dimensions game))

(defn computer-player [game]
  (let [[_ _ right bottom] (game-area game)]
    {:id       (rand-int 100000)
     :type     :computer
     :position [(rand-int right) (rand-int bottom)]
     :angle    0
     :speed    (+ (rand-int 5) 1)
     :health   2500}))

(defn player [game]
    {:id       :user
     :position (geometry/center-of (game-area game))
     :type     :player
     :angle    0
     :speed    0
     :health   2500})

(defn add-user-player [game]
  (add-player game (player game)))

(defn add-computer-players [game number]
  (reduce add-player game (take number (repeatedly (partial computer-player game)))))

(defn valid-game-position [game [x y]]
  (let [game-area (game-area game)]
    [(max 0 (min x (geometry/width game-area)))
     (max 0 (min y (geometry/height game-area)))]))

;;;;

(def game-area-width 2500)
(def game-area-height 1500)

(defn initial-game-state [game]
  (let [visible-area (:canvas-area game)]
    (-> {:game-dimensions (geometry/rectangle 0 0 game-area-width game-area-height)
         :canvas-area     visible-area
         :mouse-position  (geometry/center-of visible-area)
         :players         {}
         :bullets         {}
         :brake           :off}
        (add-user-player)
        (add-computer-players 2))))