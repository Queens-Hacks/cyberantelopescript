(ns cas.world)


(def passability {:stone false
                  :dirt false
                  :grass false
                  :sky true})

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
                (vec (concat (repeat (- bound dirt)
                               (tile :stone))
                       (repeat dirt  (tile :dirt))
                       (repeat (- height bound)
                         (tile :sky))))))]
    (vec (map new-column mrange)))))
