(ns cas.client
  (:require [cas.world :as world]))

(enable-console-print!)

(def state (atom {:world []
                  :player {:pos [0 0]
                           :mov-time [0 0]}}))
(defn setup! []
  (let [world (world/new-simple-world 80 60 20 30 40)]
    (swap! state assoc :world world)))

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




(defn draw-world! [world {:keys [ctx]}]
  (let [on-cell (fn [x y it]
                  (draw-rect! ctx (:color it) (* x 10) (* y 10) 10 10))
        on-row (fn [y row]
                 (dorun (map-indexed #(on-cell %1 y %2) row)))]
    (dorun (map-indexed on-row world))))


(defn draw-player! [player {:keys [ctx]}]
  (draw-rect! ctx [0 0 0] (* 10 10) (* 10 10) 20 20))

(defn render-state! []
  (let [canvas (.getElementById js/document "game")]
    {:canvas canvas
     :ctx (.getContext canvas "2d")
     :height (.-height canvas)
     :width (.-width canvas)}))

(defn render! []
  (let [rs (render-state!)]
    (draw-world! (:world @state) rs)
    (draw-player! (:player @state) rs)))

(defn tick! []
  (render!))

(js/setInterval tick! 1000)
(setup!)
