(ns cas.client)
(enable-console-print!)

(def state (atom 0))

(defn trace [x]
  (println x)
  x)

;; Take rgb and produce a hex color
(defn pad [char width base]
  (str (apply str (repeat (- width (.-length base)) char)) base))

(defn hex2d [n]
  (pad "0" 2 (.toString n 16)))

(defn hex-color [r g b]
  (str "#" (hex2d r) (hex2d g) (hex2d b)))

(defn draw-rect! [ctx color x y width height]
  (set! (.-fillStyle ctx) (apply hex-color color))
  (.fillRect ctx x y width height))

(defn draw-world! [world]
  (let [canvas (.getElementById js/document "game")
        ctx (.getContext canvas "2d")
        height (.-height canvas)
        width (.-width canvas)
        on-cell (fn [x y it]
                  (draw-rect! ctx it (* x 10) (* y 10) 10 10))
        on-row (fn [y row]
                 (dorun (map-indexed #(on-cell %1 y %2) row)))]
    (dorun (map-indexed on-row world))))

(let [a [100 0 0]
      b [0 100 0]
      c [0 0 100]]
  (draw-world!
    [[a a a a a]
     [b b b b b]
     [c c c c c]]))
