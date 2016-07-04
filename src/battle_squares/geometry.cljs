(ns battle-squares.geometry)

(defn position [x y]
  [x y])

(defn x-pos [position]
  (first position))

(defn y-pos [position]
  (second position))

(defn rectangle [left top right bottom]
  [left top right bottom])

(defn width [rect]
  (- (nth rect 2) (nth rect 0)))

(defn height [rect]
  (- (nth rect 3) (nth rect 1)))

(defn center-of [[left top right bottom]]
  (position (+ left (quot (- right left) 2))
            (+ top (quot (- bottom top) 2))))

(defn position-move-by [[x y] angle distance]
  [(+ x (int (Math/floor (* distance (Math/cos angle)))))
   (+ y (int (Math/floor (* distance (Math/sin angle)))))])

(defn angle-for-movement [[x y]]
  (Math/atan2 y x))
