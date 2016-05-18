(ns battle-squares.core
  (:require
   [goog.dom :as dom]
   [goog.events :as events]))

(enable-console-print!)

;(def log (.-log js/console))

(def move-speed 10)


(def animation-frame
  (or (.-requestAnimationFrame js/window)
      (.-webkitRequestAnimationFrame js/window)
      (.-mozRequestAnimationFrame js/window)
      (.-oRequestAnimationFrame js/window)
      (.-msRequestAnimationFrame js/window)
      (fn [callback] (js/setTimeout callback 17))))

(println "This text is printed from src/battle-squares/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defn random-alien []
  {:position [(rand-int 1501) (rand-int 751)]
   :movement [(rand-int 3) (rand-int 3)]})


(defonce app-state (atom {:player {:position [750 375]
                                   :movement [0 0]}
                          :bullets (list)
                          :mouse-position [750 375]
                          :aliens (take 10 (repeatedly random-alien))}))

(def player-size 50)

(defn angle-for-movement [[x y]]
  (Math/atan2 y x))

(defn next-position [[x y] angle distance]
  [(+ x (* distance (Math/cos angle))) (+ y (* distance (Math/sin angle)))])

(defn draw-line-at-angle [context [x y] angle length]
  (.beginPath context)
  (set! (. context -lineWidth) 4)
  (set! (. context -strokeStyle) "#AA0000")
  (.moveTo context x y)
  (.lineTo context (+ x (* length (Math/cos angle))) (+ y (* length (Math/sin angle))))
  (.stroke context))

(defn draw-turret [context position angle]
  (draw-line-at-angle context position angle 35))

(defn draw-player [context {position :position movement :movement}]
      (set! (.-fillStyle context) "Blue")
      (.fillRect context (- (first position) 25) (- (second position) 25) player-size player-size)
      ;(set! (.-fillStyle context) "Red")
      ;(.fillRect context (- (first position) 2) (- (second position) 35) 4 35)
      (draw-turret context position (angle-for-movement movement))
      )

(defn draw-bullet [context {position :position}]
  ;(println "draw bullet")
  (.beginPath context)
  (set! (.-fillStyle context) "Red")
  (.arc context (first position) (second position) 4 0 (* Math/PI 2) false)
  (.fill context))

(defn draw-bullets [context bullets]
  ;(println bullets)
  (doseq [bullet bullets]
    (draw-bullet context bullet)))

(defn draw-alien [context {position :position movement :movement hit :hit}]
      (set! (.-fillStyle context) (if hit "Red" "Green"))
      (.fillRect context (- (first position) 25) (- (second position) 25) player-size player-size)
      ;(set! (.-fillStyle context) (when hit "Black" "Red"))
      ;(.fillRect context (- (first position) 2) (- (second position) 35) 4 35)
      (draw-turret context position (angle-for-movement movement))
      )

(defn draw-aliens [context aliens]
  (doseq [alien aliens]
    (draw-alien context alien)))

(defn render []
  ;(println "render")
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

(defn move-player-left []
  (swap! app-state (fn [current-state]
                     (assoc-in current-state [:player :x] (- (get-in current-state [:player :x]) move-speed)))))
(defn move-player-right []
  (swap! app-state (fn [current-state]
                     (assoc-in current-state [:player :x] (+ move-speed (get-in current-state [:player :x]))))))
(defn move-player-up []
  (swap! app-state (fn [current-state]
                     (assoc-in current-state [:player :y] (- (get-in current-state [:player :y]) move-speed)))))
(defn move-player-down []
  (swap! app-state (fn [current-state]
                     (assoc-in current-state [:player :y] (+ move-speed (get-in current-state [:player :y]))))))

(defn update-game-state-rnd []
   (let [game-state app-state
         number     (rand-int 5)]
     (case number
       4 (move-player-left)
       3 (move-player-right)
       2 (move-player-up)
       1 (move-player-down)
       :do-nothing)))


(defn movement [x y]
    [x y])

(defn more-left [amount]
   (swap! app-state (fn [current-state]
                      (let [current-movement (get-in current-state [:player :movement])]
                        (assoc-in current-state [:player :movement]
                          (movement (max (* -1 (or amount 1)) -5) (second current-movement)))))))
(defn more-right [amount]
   (swap! app-state (fn [current-state]
                      (let [current-movement (get-in current-state [:player :movement])]
                        (assoc-in current-state [:player :movement]
                          (movement (min (or amount 1) 5) (second current-movement)))))))

(defn more-up [amount]
   (swap! app-state (fn [current-state]
                      (let [current-movement (get-in current-state [:player :movement])]
                        (assoc-in current-state [:player :movement]
                          (movement (first current-movement) (max (* -1 (or amount 1)) -5)))))))

(defn more-down [amount]
   (swap! app-state (fn [current-state]
                      (let [current-movement (get-in current-state [:player :movement])]
                        (assoc-in current-state [:player :movement]
                          (movement (first current-movement) (min (or amount 1) 5)))))))

(defn move-player []
   (swap! app-state (fn [current-state]
                      (let [{:keys [position movement]} (:player current-state)]
                        (assoc-in current-state [:player :position]
                          [(mod (+ (first position) (first movement)) 1500) (mod (+ (second position) (second movement)) 750)])))))


(defn stop-x []
   (swap! app-state (fn [current-state]
                      (let [current-movement (get-in current-state [:player :movement])]
                        (assoc-in current-state [:player :movement]
                          (movement 0 (second current-movement)))))))

(defn stop-y []
   (swap! app-state (fn [current-state]
                      (let [current-movement (get-in current-state [:player :movement])]
                        (assoc-in current-state [:player :movement]
                          (movement (first current-movement) 0))))))

(defn update-player-movement []
  (let [current-state       @app-state
        [player-x player-y] (get-in current-state [:player :position])
        [mouse-x mouse-y]   (get current-state :mouse-position)]
      (when (< mouse-x player-x) (more-left (+ 1 (quot (- player-x mouse-x) 20))))
      (when (> mouse-x player-x) (more-right (+ 1 (quot (- mouse-x player-x) 20))))
      (when (= mouse-x player-x) (stop-x))
      (when (< mouse-y player-y) (more-up (+ 1 (quot (- player-y mouse-y) 20))))
      (when (> mouse-y player-y) (more-down (+ 1 (quot (- mouse-y player-y) 20))))
      (when (= mouse-y player-y) (stop-y))))

(defn update-bullet [{:keys [position angle life]}]
  ;(println "update-bullet")
  (when (< life 30)
    {:position (next-position position angle 20)
     :angle angle
     :life (+ life 1)}))

(defn update-bullets [bullets]
  (filter identity
    (map update-bullet bullets)))

(defn update-bullets-game-state []
  ;(println "u-b-g-s")
  (swap! app-state
    (fn [current-state]
      (assoc current-state
        :bullets (update-bullets (:bullets current-state))))))

(defn hit-check [[x y] size bullet]
  (let [[other-x other-y] (:position bullet)]
  ;(println x y size other-x other-y)
      (and (> other-x (- x size))
           (< other-x (+ x size))
           (> other-y (- y size))
           (< other-y (+ y size)))))

(defn hit-by-bullet [position bullets]
  (when bullets
    ;(println position bullets)
    (first (filter identity
      (map (partial hit-check position 25) bullets)))))

(defn update-alien [bullets {:keys [position movement hit] :as alien}]
  ;(println alien bullets)
  (let [hit       (or hit (hit-by-bullet position bullets))
        dead-time (or (:dead-time alien) 0)]
    (when (< dead-time 30)
        {:position  (if hit
                      position
                      [(mod (+ (first position) (first movement)) 1500) (mod (+ (second position) (second movement)) 750)])
         :hit       hit
         :dead-time (if hit (+ dead-time 1) dead-time)
         :movement  (if hit [0 0] movement)}
         )))

(defn update-aliens [current-state]
  (let [aliens (:aliens current-state)]
    (filter identity (map (partial update-alien (:bullets current-state)) aliens))))

(defn update-aliens-game-state []
  ;(println "u-b-g-s")
  (swap! app-state
    (fn [current-state]
      (assoc current-state
        :aliens (update-aliens current-state)))))


(defn update-game-state []
  ;(println "u-g-s")
  (update-bullets-game-state)
  (update-player-movement)
  (update-aliens-game-state)
  (move-player))

(defn tick []
    ;(update-game-state-rnd)
    ;(move-player-right)
    (update-game-state)
    (js/setTimeout tick 17))

;(animation-frame render)

;(tick)

(def max-speed 5)


;(defn handle-input [character]
 ; (case character
  ;  87 (more-up) ;(move-player-up)
   ; 65 (more-left) ;(move-player-left)
;    83 (more-down) ;(move-player-down)
 ;   68 (more-right) ;(move-player-right)
  ;  :do-nothing))

(defn handle-mouse-move [mouse-event]
  ;(.log js/console mouse-event)
  (let [mouse-x (aget mouse-event "clientX")
        mouse-y (aget mouse-event "clientY")]
  (swap! app-state (fn [current-state] (assoc current-state :mouse-position [mouse-x mouse-y])))))

(defn bullet [player]
  {:position (:position player)
   :angle    (angle-for-movement (:movement player))
   :life     0
   :speed    15})


(defn fire-bullet []
  (swap! app-state
    (fn [current-state]
        (assoc current-state :bullets (conj (:bullets current-state) (bullet (:player current-state)))))))



(defn shoot []
  (fire-bullet))


(defn handle-mouse-click [mouse-click]
  ;(.log js/console mouse-click)
  (shoot))


;(events/listen (dom/getWindow)
 ;   (.-KEYDOWN events/EventType) #(handle-input (aget % "keyCode")))

(defonce event-setup
    (do
        (events/listen (dom/getWindow)
            (.-MOUSEMOVE events/EventType) #(handle-mouse-move %))
        (events/listen (dom/getWindow)
            (.-CLICK events/EventType) #(handle-mouse-click %))))

(defonce start-game
  (do
    (animation-frame render)
    (tick)))





(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
