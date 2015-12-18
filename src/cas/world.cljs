(ns cas.world)


(def passability {:stone false
                  :dirt false
                  :grass false
                  :air true})

(defn tile
  [kind]
  {:kind kind
   :passable (passability kind)})

(defn rand-in-range
  [lower upper]
  (+ (rand-int (- upper lower)) lower))

(defn limit-range
  [value lower upper]
  (cond (< value lower) lower
        (> value upper) upper
        :else value))

(defn new-mountain-range
  [length lower upper]
  (loop [last-level (rand-in-range lower upper)
         result [last-level]]
    (if (= (count result) length)
      result
      (let [next (let [up-or-down (- (rand-int 5) 2)]
                     (limit-range (+ last-level up-or-down)
                                  lower
                                  upper))]
        (recur next (conj result next))))))

(defn new-mountain-world
  [width height lower upper]
  (let [mrange (new-mountain-range width lower upper)]
    (letfn [(new-column [bound]
              (let [dirt (rand-in-range 1 4)]
                (vec (concat (repeat (- height bound)
                                     (tile :air))
                             (repeat dirt  (tile :dirt))
                             (repeat (- bound dirt)
                                     (tile :stone))))))]
      (vec (map new-column mrange)))))

(defn new-coord-map
  [width height]
  (letfn [(new-col [x]
            (vec (map (fn [y] [x y]) (range height))))]
  (vec (map new-col (range width)))))

(defn new-simplex-map
  [width height]
  (let [simpl (js/SimplexNoise. rand)
        coords (new-coord-map width height)
        freq (/ 5.0 width)]
    (letfn [(simplex-tile [coord]
              (.noise2D simpl
                        (* freq (coord 0))
                        (* freq (coord 1))))
            (simplex-col [col]
              (vec (map simplex-tile col)))]
      (vec (map simplex-col coords)))))

(defn apply-linear-gradient
  [grid height]
  (let [grad (vec (map #(* % (/ 1.0 height))
                       (range height)))]
    (vec (map #(vec (map + % grad)) grid))))

(defn grid-to-stone
  [grid]
  (vec (map (fn [col] (vec (map (fn [cell]
                                  (tile (if (<= cell 0.5)
                                    :air
                                    :stone))
                        )
                      col)))
       grid)))

(defn new-chunk
  [width height]
  (let [simplex-map (new-simplex-map width height)
        weighted-map (apply-linear-gradient simplex-map height)]
    (grid-to-stone weighted-map)))

;(enable-console-print!)
;(println "about to simplex")
;(println "simplex map" (new-simplex-map 10 20))
;(println "done simplex")
