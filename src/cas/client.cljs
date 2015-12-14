(ns cas.client
  (:require [cas.world :as world]))

(enable-console-print!)

(def world-width 80)
(def world-height 60)

(def state (atom {:world []
                  :player {:pos {:x 0 :y 0}
                           :mov-time [0 0]}}))
(def keystate (atom {}))

(defn setup! []
  (let [world (world/new-mountain-world world-width world-height 30 40)]
    (swap! state assoc :world (trace world))))

(defn trace [x]
  (println x)
  x)

;; Take rgb and produce a hex color
(defn draw-rect! [ctx color x y width height]
  (set! (.-fillStyle ctx) color)
  (.fillRect ctx x y width height))

(defn draw-tile! [ctx color x y]
  (draw-rect! ctx color x y 10 10))

(defn world-at [world x y]
  (let [idx (+ x (* y world-width))]
    ((world x) (- world-height y 1))))

(defn draw-world! [world {:keys [ctx]}]
  (loop [x 0 y 0]
    (let [it (world-at world x y)]
      (draw-rect! ctx (:color it) (* x 10) (* y 10) 10 10)
      (cond
        (< x (dec world-width)) (recur (inc x) y)
        (< y (dec world-height)) (recur 0 (inc y))
        :else nil))))


;; (defn draw-world! [world {:keys [ctx]}]
;;   (let [on-cell (fn [x y it]
;;                   (draw-rect! ctx (:color it) (* x 10) (* y 10) 10 10))
;;         on-row (fn [y row]
;;                  (dorun (map-indexed #(on-cell %1 y %2) row)))]
;;     (dorun (map-indexed on-row world))))

(defn draw-player! [{:keys [pos]} {:keys [ctx]}]
  (draw-rect! ctx "#000" (* (:x pos) 10) (* (:y pos) 10) 10 10))

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

(defn update-player [world {:keys [pos mov-time] :as old-player}]
  (if (:passable (world-at world (:x pos) (inc (:y pos))))
    (update-in old-player [:pos :y] #(inc %))
    old-player))

(defn update-player! []
  (swap! state assoc :player (update-player (:world @state)
                                            (:player @state))))

(defn update! []
  (update-player!))

(defn is-key-down [key]
  (@keystate key))

(.addEventListener js/document "keydown"
  (fn [e] (swap! keystate assoc (.-keyCode e) true)))

(.addEventListener js/document "keyup"
  (fn [e] (swap! keystate assoc (.-keyCode e) false)))

;; Set up the event loop
(defn tick! []
  (let [start (.now js/Date)]
    (update!)
    (render!)
    (println (- (.now js/Date) start))))
(js/setInterval tick! 100)

;; Set up the basic program structure
(setup!)


;; Movement handling logic
(defn walk! [dir]
  (let [op (case dir :left dec :right inc)
        world (:world @state)
        player (:player @state)
        pos (:pos player)]
    (cond
      (:passable (world-at world (op (:x pos)) (:y pos)))
      (swap! state assoc :player (update-in player [:pos :x] op))
      (:passable (world-at world (op (:x pos)) (dec(:y pos))))
      (swap! state assoc :player (update-in (update-in player [:pos :y] dec) [:pos :x] op))
      :else player)))

(def movekeymap (atom {}))
(defn start-walking! [dir]
  (if-not (dir @movekeymap)
    (do (walk! dir)
        (swap! movekeymap assoc dir (js/setInterval #(walk! dir) 100)))))
(.addEventListener js/document "keydown"
  (fn [e]
    (case (.-keyCode e)
      37 (start-walking! :left)
      39 (start-walking! :right)
      nil)))

(defn stop-walking! [dir]
  (js/clearInterval (dir @movekeymap))
  (swap! movekeymap assoc dir nil))
(.addEventListener js/document "keyup"
  (fn [e]
    (case (.-keyCode e)
      37 (stop-walking! :left)
      39 (stop-walking! :right)
      nil)))
