(ns cas.world
  (:require [cas.util :refer [tile-at each-tile map-tiles]]))


(def passability {:stone false
                  :dirt false
                  :grass false
                  :air true
                  :ore false})

(defn mk-tile
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
                                     (mk-tile :air))
                             (repeat dirt  (mk-tile :dirt))
                             (repeat (- bound dirt)
                                     (mk-tile :stone))))))]
      (vec (map new-column mrange)))))

(defn new-coord-map
  [width height]
  (letfn [(new-col [x]
            (map #(vector x %) (range height)))]
  (map new-col (range width))))

(defn new-simplex-map
  [width height [densx densy] transformer]
  (let [simpl (js/SimplexNoise. rand)
        coords (new-coord-map width height)
        freqx (/ densx width)
        freqy (/ densy height)]
    (letfn [(simplex-tile [[x y]]
              (let [noise (.noise2D simpl (* freqx x) (* freqy y))
                    noise_prime (transformer x y noise)]
                noise_prime))
            (simplex-col [col]
              (vec (map simplex-tile col)))]
      (vec (map simplex-col coords)))))

(defn ores-transform
  [tiles x y noise]
  (let [tile (tile-at tiles x y)]
    (if (and (= (:kind tile) :stone) (< noise -0.7))
      (mk-tile :ore)
      tile)))

(defn hills-transform
  [x y noise]
  (def BASE-NUMBER 50)
  (def CENTER-NUMBER 15)
  (let [adjusted-noise
        (if (< y BASE-NUMBER)
          -1 ;; Magically make this stuff air!?!?!?!?
          (let [extremeness (/ (- y BASE-NUMBER CENTER-NUMBER) CENTER-NUMBER)]
            (+ noise extremeness)))]
    (mk-tile
      (cond
        (< adjusted-noise 0) :air
        (< adjusted-noise 0.15) :dirt
        :else :stone))))

(defn grow-grass
  [tiles x y tile]
  (if (and
        (= (:kind tile) :dirt)
        (= (:kind (tile-at tiles x (dec y))) :air))
    (mk-tile :grass)
    tile))

(defn foo [x]
  (/ (+ 1 x) 2))

(defn caves-transform
  [tiles noise-1 x y noise]
  (let [tile (tile-at tiles x y)]
    (if (< (* (foo (tile-at noise-1 x y)) noise) -0.25)
      (mk-tile :air)
      tile)))

;; (defn caves-transform
;;   [tiles x y noise]
;;   (let [tile (tile-at tiles x y)]
;;     (if (< noise -0.25)
;;       (mk-tile :air)
;;       tile)))

(defn new-chunk
  [width height]
  (let [land-tiles (new-simplex-map width height [3 2]
                     hills-transform)
        grassy-tiles (map-tiles land-tiles (partial grow-grass land-tiles))
        ores-map (new-simplex-map width height [20 20]
                   (partial ores-transform grassy-tiles))
        caves-noise-1 (new-simplex-map width height [3 3] (fn [_ _ n] n))
        caves-map (new-simplex-map width height [10 10]
                        (partial caves-transform ores-map caves-noise-1))

        ;; caves-map (new-simplex-map width height [10 10]
        ;;             (partial caves-transform ores-map))
        ]
    caves-map))

