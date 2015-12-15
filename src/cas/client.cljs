(ns cas.client
  (:require [cas.world :as world]
            [cas.state :refer [state world-width world-height tile-dim]]
            [cas.render :refer [renderer stage tile-textures robot-texture]]))

(enable-console-print!)

;; XXX: Don't hard-code the world's width ane height
(def keystate (atom {}))

;; Helper functions for traversing the world's state tile-by-tile
(defn each-tile [world func]
  (loop [x 0 y 0]
    (let [it (world-at world x y)]
      (func it x y)
      (cond
        (< x (dec world-width)) (recur (inc x) y)
        (< y (dec world-height)) (recur 0 (inc y))
        :else nil))))
(defn map-tiles [world func]
  (vec (map-indexed (fn [x col]
                      (vec (map-indexed #(func x %1 %2) col)))
         world)))
(defn world-at [world x y] ((world x) (- world-height y)))

(defn setup! []
  (let [world (world/new-mountain-world world-width world-height 5 20)]
    (swap! state assoc :world
      (map-tiles world
        (fn [x y tile]
          (let [tex ((:kind tile) tile-textures)
                sprite (js/PIXI.Sprite. tex)]
            ;; Add the sprite to the stage
            (.addChild stage sprite)
            (println (:kind tile) tile-dim)
            (set! (.-x sprite) (* x tile-dim))
            (set! (.-y sprite) (* (- world-height y) tile-dim))
            (set! (.-width sprite) tile-dim)
            (set! (.-height sprite) tile-dim)
            ;; Associate the sprite with the tile
            (assoc tile :sprite sprite)))))
    ;; Create the player's sprite
    (let [psprite (js/PIXI.Sprite. robot-texture)]
      (swap! state assoc :psprite psprite)
      (.addChild stage psprite)
      (set! (.-width psprite) tile-dim)
      (set! (.-height psprite) tile-dim))))

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
  (update!))
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
      (:passable (world-at world (op (:x pos)) (dec (:y pos))))
      (swap! state assoc :player (update-in (update-in player [:pos :y] dec) [:pos :x] op))
      (not (:passable (world-at world (op (:x pos)) (dec (:y pos)))))
      (swap! state assoc :player (update-in player [:pos :y] #(- % 2)))
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


(defn render-tick! []
  (js/requestAnimationFrame render-tick!)
  (let [sprite (:psprite @state)
        {:keys [x y]} (:pos (:player @state))]
    (set! (.-x sprite) (* x tile-dim))
    (set! (.-y sprite) (* y tile-dim)))
  (.render renderer stage))
(render-tick!)
