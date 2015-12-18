(ns cas.util)

(defn tile-at [world x y] ((world x) y))
;; Helper functions for traversing the world's state tile-by-tile
(defn each-tile [world func]
  (loop [x 0 y 0]
    (let [it (tile-at world x y)
          width (count world)
          height (count (world y))]
      (func it x y)
      (cond
        (< x (dec width)) (recur (inc x) y)
        (< y (dec height)) (recur 0 (inc y))
        :else nil))))
(defn map-tiles [world func]
  (vec (map-indexed (fn [x col]
                      (vec (map-indexed #(func x %1 %2) col)))
         world)))
