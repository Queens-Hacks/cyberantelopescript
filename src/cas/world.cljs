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

(defn apply-linear-gradient
  [grid height]
  (let [grad (vec (map #(* % (/ 1.0 height))
                       (range height)))]
    (vec (map #(vec (map + % grad)) grid))))

(defn ores-transform
  [tiles x y noise]
  (let [tile (tile-at tiles x y)]
    (if (and (= (:kind tile) :stone) (< noise -0.7))
      (mk-tile :ore)
      tile)))

(defn hills-caves-transform
  [x y noise]
  (def BASE-NUMBER 20)
  (def RANDOM_NUMBER 50)
  (def CENTER-NUMBER (/ RANDOM_NUMBER 2))
  (if (< y BASE-NUMBER)
    -1
    (let [y (- y BASE-NUMBER)
          norm-noise (/ (+ noise 1) 2)]
      (if (< y RANDOM_NUMBER)
        (let [extremeness (/ (js/Math.abs (- y CENTER-NUMBER)) CENTER-NUMBER)
              noisyness (- 1 extremeness)
              adjusted-noise (* norm-noise noisyness)
              is-air (< y CENTER-NUMBER)]
          (cond
            is-air (- noise extremeness)
            (= y CENTER-NUMBER) noise
            :else (+ noise extremeness)))
        1))))

(defn noise-to-tiles
  [x y noise]
  (mk-tile
    (cond
      (< noise 0) :air
      (< noise 0.15) :dirt
      :else :stone)))

(defn grow-grass
  [tiles x y tile]
  (if (and
        (= (:kind tile) :dirt)
        (= (:kind (tile-at tiles x (dec y))) :air))
    (mk-tile :grass)
    tile))

(defn new-chunk
  [width height]
  (let [simplex-map (new-simplex-map width height [3 2]
                      hills-caves-transform)
        tiles (map-tiles simplex-map noise-to-tiles)
        grassy-tiles (map-tiles tiles (partial grow-grass tiles))
        ores-map (new-simplex-map width height [20 20]
                   (partial ores-transform grassy-tiles))]
    ores-map))

;(enable-console-print!)
;(println "about to simplex")
;(println "simplex map" (new-simplex-map 10 20))
;(println "done simplex")
