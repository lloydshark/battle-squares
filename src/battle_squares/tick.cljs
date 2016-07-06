(ns battle-squares.tick
  (:require [battle-squares.geometry :as geometry]
            [battle-squares.model :as model]))

;; Game Tick...

(defn hit-check [player bullet]
  (let [[x y]             (:position player)
        size              (model/player-size player)
        [other-x other-y] (:position bullet)]
    (when (and (not (= (:owner bullet) (:id player)))
               (> other-x (- x size))
               (< other-x (+ x size))
               (> other-y (- y size))
               (< other-y (+ y size)))
      bullet)))

(defn hit-by-bullet [player bullets]
  (when bullets
    (first (filter identity
                   (map (partial hit-check player) bullets)))))

(defn speed-for-movement [x y x2 y2]
  (if (> (Math/sqrt (+ (* (- x x2)(- x x2)) (* (- y y2) (- y y2)))) 40) 7 0))

(defn update-player [game player]
  (assoc-in game [:players (:id player)] player))

(defn remove-player [game player]
  (update-in game [:players] dissoc (:id player)))

(defn update-user-movement [game]
  (let [visible-area              (model/calculate-visible-area game)
        [raw-mouse-x raw-mouse-y] (:mouse-position game)
        [mouse-x mouse-y]         [(+ raw-mouse-x (first visible-area))
                                   (+ raw-mouse-y (second visible-area))]
        player              (get-in game [:players :user])
        [player-x player-y] (:position player)]
    (if (:dead player)
      (update-player game (assoc player :speed 0))
      (update-player game (assoc player :angle (geometry/angle-for-movement [(- mouse-x player-x) (- mouse-y player-y)])
                                        :speed (if (= (:brake game) :on)
                                                 0
                                                 (min (speed-for-movement mouse-x mouse-y player-x player-y) 20)))))))

(defn collision-test [player new-position other-player]
  (let [[player-x player-y]             new-position
        the-player-size                 (model/player-size player)
        [other-player-x other-player-y] (:position other-player)
        other-player-size               (model/player-size other-player)
        collision?                      (and (not= (:id player) (:id other-player))
                                             (>= (+ (quot the-player-size 2) (quot other-player-size 2))
                                                 (+ (js/Math.abs (- player-x other-player-x))
                                                    (js/Math.abs (- player-y other-player-y)))))]
    collision?))

;(defn collision? [game player-id new-position]
;  (let [player (get-in game [:players player-id])]
;    (first (filter identity (map (partial collision-test player new-position) (vals (:players game)))))))

;(defn prevent-collision [game player-id position new-position]
;  (if (collision? game player-id new-position) position new-position))

(defn move-user-player [game]
  (let [{:keys [position angle speed dead dead-time] :as player} (get-in game [:players :user])]
    (cond
      (not dead) (update-player game (assoc player :position (model/valid-game-position game
                                                                                        (geometry/position-move-by position angle speed))))
      dead       (update-player game (assoc player :dead-time (+ 1 dead-time))))))

(defn tick-user-player [game]
  (-> (update-user-movement game)
      (move-user-player)))

(defn maybe-shoot [game player-id]
  (if (> (rand-int 1000) 995)
    (model/player-shoot game player-id)
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
                                                    :position (model/valid-game-position game
                                                                                         (geometry/position-move-by position angle speed))))
                                   (maybe-shoot (:id computer-player)))
        (and dead
             (<= dead-time 30)) (update-player game (assoc computer-player :dead true
                                                                           :dead-time (+ 1 dead-time)
                                                                           :speed 0))
        (and dead
             (> dead-time 30)) (-> (remove-player game computer-player)
                                   (model/add-player (model/computer-player game)))))
    game))

(defn computer-chase-player [game computer-player-id]
  (if-let [computer-player        (get-in game [:players computer-player-id])]
    (let [[computer-x computer-y] (:position computer-player)
          user-player             (get-in game [:players :user])
          [player-x player-y]     (:position user-player)]
      (update-player game (assoc computer-player
                            :angle (geometry/angle-for-movement [(- player-x computer-x) (- player-y computer-y)]))))
    game))

(defn computer-player-think [game computer-player-id]
  (if (> (rand-int 5000) 4900)
    (computer-chase-player game computer-player-id)
    game))

(defn tick-computer-player [game computer-player-id]
  (-> (computer-player-think game computer-player-id)
      (update-computer-player computer-player-id)))


(defn tick-computer-players [game]
  (reduce tick-computer-player game (model/computer-players game)))

(defn update-bullet [game bullet]
  (assoc-in game [:bullets (:id bullet)] bullet))

(defn remove-bullet [game bullet]
  (update-in game [:bullets] dissoc (:id bullet)))

(defn tick-bullet [game {:keys [position angle life] :as bullet}]
  (if (< life 30)
    (update-bullet game
                   (assoc bullet :position (model/valid-game-position game (geometry/position-move-by position angle 20))
                                 :life     (+ life 1)))
    (remove-bullet game bullet)))

(defn tick-bullets [game]
  (reduce tick-bullet game (vals (:bullets game))))

;; Bullet Hit

(defn damage-from-bullet [game player bullet]
  (let [damage (model/damage bullet)
        health (max (- (:health player) damage) 0)
        dead   (= health 0)]
    [(update-player game (assoc player :health health
                                       :dead   dead))
     damage]))

(defn reward-health-for-hit [[game damage] bullet]
  (if-let [owner (model/get-player game (model/owner-of bullet))]
    (if (and owner (model/alive? owner))
      (update-player game (assoc owner :health (+ damage (:health owner))))
      game)
    game))

(defn update-player-hit [game player bullet]
  (-> (damage-from-bullet game player bullet)
      (reward-health-for-hit bullet)
      (remove-bullet bullet)))

(defn bullet-hit-detect [game player]
  (if-let [bullet (hit-by-bullet player (model/bullets game))]
    (update-player-hit game player bullet)
    game))

(defn tick-bullet-hit-detect [game]
  (reduce bullet-hit-detect game (model/players game)))

;; Player Collisions

(defn player-collision? [player other-player]
  (let [[player-x player-y]             (model/position player)
        the-player-size                 (model/player-size player)
        [other-player-x other-player-y] (:position other-player)
        other-player-size               (model/player-size other-player)
        collision?                      (and (not= (:id player) (:id other-player))
                                             (>= (+ (quot the-player-size 2) (quot other-player-size 2))
                                                 (+ (js/Math.abs (- player-x other-player-x))
                                                    (js/Math.abs (- player-y other-player-y)))))]
    collision?))

(defn player-collision [player other-players]
  (first (filter (partial player-collision? player) other-players)))

(defn update-player-collision
  "The bigger player takes health from the smaller when they collide."
  [game player other-player]
  (let [biggest       (if (> (model/player-size player) (model/player-size other-player)) player other-player)
        smallest      (if (> (model/player-size player) (model/player-size other-player)) other-player player)
        max-damage    (model/collision-damage biggest)
        actual-damage (min max-damage (model/health smallest))]
    (-> (model/decrease-player-health game (:id smallest) actual-damage)
        (model/increase-player-health (:id biggest) actual-damage))))

(defn player-collision-detect [game player]
  (let [player       (model/get-player game (:id player))
        other-player (when player (player-collision player (model/players game)))]
    (if other-player
      (update-player-collision game player other-player)
      game)))

(defn tick-player-collision-detect [game]
  (reduce player-collision-detect game (model/players game)))


(defn tick-game [game]
  (if (model/game-over? game)
    (model/initial-game-state game)
    (-> (tick-user-player game)
        (tick-computer-players)
        (tick-bullets)
        (tick-bullet-hit-detect)
        (tick-player-collision-detect))))