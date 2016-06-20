(ns battle-squares.dom)

(defn element-by-id [id]
  (.getElementById js/document id))
