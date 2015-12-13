(ns cas.world)

(def tile-colors {:stone [125 117 114]
                  :dirt [125 97 87]
                  :grass [96 125 87]
                  :sky [87 115 125]})

(defn tile
  [kind]
  {:kind kind :color (tile-colors kind)})

(defn new-simple-world
  [x y stone-to-dirt dirt-to-grass grass-to-sky]
  (letfn [(new-row [size kind]
            (vec (repeatedly size #(tile kind))))
          (kind [level] (let [level (dec (- y level))]
            (cond (< level stone-to-dirt) :stone
                  (< level dirt-to-grass) :dirt
                  (< level grass-to-sky) :grass
                  :else :sky)))]
    (vec (map #(new-row x (kind %)) (range y)))))
