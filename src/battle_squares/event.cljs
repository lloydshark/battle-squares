(ns battle-squares.event
  (:require [battle-squares.model :as model]))

(defn handle-mouse-move [game-state mouse-event]
  (let [mouse-x (aget mouse-event "clientX")
        mouse-y (aget mouse-event "clientY")]
    (swap! game-state model/update-mouse-position mouse-x mouse-y)))

(defn handle-mouse-click [game-state _]
  (swap! game-state model/user-shoot))

(def SPACEBAR 32)

(defn handle-keydown [game-state keycode]
  (when (= SPACEBAR keycode)
    (swap! game-state model/toggle-brake)))

;; Use wrapper functions so that we can hot-reload updated handlers...
(defn attach-handle-mouse-move [game-state event] (handle-mouse-move game-state event))
(defn attach-handle-mouse-click [game-state event] (handle-mouse-click game-state event))
(defn attach-handle-keydown [game-state keycode] (handle-keydown game-state keycode))