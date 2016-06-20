(ns battle-squares.geometry)


(defn rectangle [top-left bottom-right]
  [top-left bottom-right])

(defn width [rect]
  (- (nth rect 2) (nth rect 0)))

(defn height [rect]
  (- (nth rect 3) (nth rect 1)))

(defn position [x y]
  [x y])
