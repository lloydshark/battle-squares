(ns battle-squares.core
  (:require
    [battle-squares.dom :refer [element-by-id]]
    [battle-squares.event :as event]
    [battle-squares.geometry :as geometry]
    [battle-squares.model :as model]
    [battle-squares.renderer :as renderer]
    [battle-squares.tick :as tick]
    [battle-squares.util :as util]
    [goog.dom :as dom]
    [goog.events :as events]))

(enable-console-print!)

;; Game Tick...

(defn game-ticker [game-state]
  (swap! game-state tick/tick-game)
  (js/setTimeout #(game-ticker game-state) 17))

;; Event Setup...

(defn event-setup [game-state]
  (events/listen (dom/getWindow)
      (.-MOUSEMOVE events/EventType) #(event/attach-handle-mouse-move game-state %))
  (events/listen (dom/getWindow)
      (.-CLICK events/EventType) #(event/attach-handle-mouse-click game-state %))
  (events/listen (dom/getWindow)
     (.-KEYDOWN events/EventType) #(event/attach-handle-keydown game-state (aget % "keyCode")))
  )

;; Canvas Setup...

(defn canvas-setup
  "Ensure that the canvas is set to match the size of its container (typically meaning full screen)..."
  [game-state canvas-element-id canvas-container-element-id]
  (let [game-canvas-holder (element-by-id canvas-container-element-id)
        game-canvas        (element-by-id canvas-element-id)
        canvas-area        (geometry/rectangle 0 0 (aget game-canvas-holder "clientWidth")
                                                   (aget game-canvas-holder "clientHeight"))]
    (set! (.-width game-canvas) (geometry/width canvas-area))
    (set! (.-height game-canvas) (geometry/height canvas-area))
    (swap! game-state model/update-canvas-area canvas-area)))

;; Rendering Setup...

(defn render-game [game-state]
  (renderer/render @game-state)
  (util/request-animation-frame #(render-game game-state)))

;; Game State...

(defn initialise-game [game-state]
  (swap! game-state #(model/initial-game-state %)))

;; One time setup...

(def canvas-element-id "gameCanvas")
(def canvas-container-element-id "gameCanvasContainer")

(defn start-game [game-state]
  (canvas-setup game-state canvas-element-id canvas-container-element-id)
  (initialise-game game-state)
  (event-setup game-state)
  (render-game game-state)
  (game-ticker game-state)
  )

(defonce game-state (atom {}))

(defonce game (start-game game-state))

;;;;;;;;;;;;;;
(comment game)
;;;;;;;;;;;;;;

;; TODO
;; Allow ships to collide and take mass from each other.
;; Make aliens evade being shot.
;; Shoot from end of turret.
;; Get slower when bigger.