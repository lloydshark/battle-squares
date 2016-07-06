(ns battle-squares.renderer
  (:require [battle-squares.model :as model]
            [battle-squares.geometry :as geometry]))

;; Render...

(defn draw-line-at-angle [context [x y] angle length width color]
  (.beginPath context)
  (set! (. context -lineWidth) width)
  (set! (. context -strokeStyle) color)
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

(defn draw-turret [context position angle length width color]
  (draw-line-at-angle context position angle length width color))

(defn player-color [player]
  (if (= :computer (:type player)) "lightgreen" "lightblue"))

(defn turret-color [player]
  (if (= :computer (:type player)) "darkgreen" "darkblue"))

(defn draw-player [context {position :position angle :angle :as player} visible-area]
  (when (area-contains visible-area position)
    (let [player-size  (model/player-size player)
          turret-width (quot player-size 8)]
      (.save context)
      ;; draw body...
      (.translate context
                  (- (first position) (first visible-area))
                  (- (second position) (second visible-area)))
      ;(.rotate context angle)
      (set! (.-fillStyle context) (player-color player))
      ;(.fillRect context (quot player-size -2) (quot player-size -2) player-size player-size)
      (.beginPath context)
      (.arc context 0 0 (quot player-size 2) 0 (* Math/PI 2) false)
      (.fill context)
      (.restore context)

      ;; draw turret...
      (draw-turret context [(- (first position)  (first visible-area))
                            (- (second position) (second visible-area))] angle (* 0.6 player-size)
                   turret-width (turret-color player)))
    ))

(defn draw-players [context players visible-area]
  (doseq [player players] (draw-player context player visible-area)))

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

(defn draw-background [context game-area visible-area]
  (draw-rect context
             (screen-coordinates (rect-intersection visible-area game-area) visible-area)))

(defn calculate-visible-terrain-lines [game-area visible-area]
  (let [[left top right bottom] (rect-intersection visible-area game-area)
        visible-horizontals     (filter #(zero? (rem % 250)) (range top bottom))
        visible-verticals       (filter #(zero? (rem % 250)) (range left right))]
    (concat
      (for [x visible-verticals] {:from {:x x :y top} :to {:x x :y bottom}})
      (for [y visible-horizontals] {:from {:x left :y y} :to {:x right :y y}}))))

(defn screen-coordinate [{:keys [x y]} visible-area]
  (let [visible-left (first visible-area)
        visible-top  (second visible-area)]
    [(- x visible-left) (- y visible-top)]))

(defn draw-terrain [context game-area visible-area]
  (doseq [line (calculate-visible-terrain-lines game-area visible-area)]
    (draw-line context
               (screen-coordinate (:from line) visible-area)
               (screen-coordinate (:to line) visible-area))))

(defn render [game]
  (let [game-area (model/game-area game)
        canvas-area (:canvas-area game)
        target  (.getElementById js/document "gameCanvas")
        context (.getContext target "2d")
        visible-area (model/calculate-visible-area game)
        players (vals (:players game))
        bullets (vals (:bullets game))]
    (.clearRect context 0 0 (geometry/width canvas-area) (geometry/height canvas-area))
    (draw-background context game-area visible-area)
    (draw-terrain context game-area visible-area)
    (draw-players context players visible-area)
    (draw-bullets context bullets visible-area)))
