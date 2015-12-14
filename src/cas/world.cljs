(ns cas.world)


(defn pad [char width base]
  (str (apply str (repeat (- width (.-length base)) char)) base))

(defn hex2d [n]
  (pad "0" 2 (.toString n 16)))

(defn hex-color [r g b]
  (str "#" (hex2d r) (hex2d g) (hex2d b)))


(def tile-colors {:stone [125 117 114]
                  :dirt [125 97 87]
                  :grass [96 125 87]
                  :sky [87 115 125]})

(def passability {:stone false
                  :dirt false
                  :grass false
                  :sky true})

(defn tile
  [kind]
  {:kind kind
   :color (apply hex-color (tile-colors kind))
   :passable (passability kind)})

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
              (vec (concat (repeat bound
                              (tile :stone))
                      (repeat (- height bound)
                              (tile :sky)))))]
    (vec (map new-column mrange)))))
