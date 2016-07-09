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

(defn distance-between-points [position-1 position-2]
  (let [[x1 y1] position-1
        [x2 y2] position-2]
    (js/Math.sqrt (+ (* (- x1 x2)(- x1 x2)) (* (- y1 y2)(- y1 y2))))))

(defn line-intersects-point-ish?
  "Length via the position should be within 10%."
  [line position]
  (let [length-of-line                   (distance-between-points (first line) (second line))
        length-of-line-90-percent        (* 0.9 length-of-line)
        length-of-line-110-percent       (* 1.1 length-of-line)
        length-via-position              (+ (distance-between-points (first line) position)
                                            (distance-between-points (second line) position))]
    (and (< length-of-line-90-percent length-via-position)
         (> length-of-line-110-percent length-via-position))))