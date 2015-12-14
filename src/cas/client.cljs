(ns cas.client
  (:require [cas.world :as world]))

(enable-console-print!)

;; Create the pixi stage
(def renderer
  (.autoDetectRenderer js/PIXI 800 600 #js {:backgroundColor 0x1099bb}))
(.appendChild (.. js/document -body) (.-view renderer))

(def stage (js/PIXI.Container.))

(defn- mk-texture [path]
  ((.. js/PIXI -Texture -fromImage) path))
(def tile-textures {:air (mk-texture "tiles/air.png")
                    :grass (mk-texture "tiles/grass.png")
                    :stone (mk-texture "tiles/stone.png")})
(def robot-texture (mk-texture "tiles/robot.png"))

;; XXX: Don't hard-code the world's width ane height
(def world-width 80)
(def world-height 60)
(def state (atom {:world []
                  :player {:pos {:x 0 :y 0}
                           :mov-time [0 0]}}))
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
  (let [world (world/new-mountain-world world-width world-height 30 40)]
    (swap! state assoc :world
      (map-tiles world
        (fn [x y tile]
          (let [tex ((:kind tile) tile-textures)
                sprite (js/PIXI.Sprite. tex)]
            ;; Add the sprite to the stage
            (.addChild stage sprite)
            (set! (.-x sprite) (* x 10))
            (set! (.-y sprite) (* (- world-height y) 10))
            (set! (.-width sprite) 10)
            (set! (.-height sprite) 10)
            ;; Associate the sprite with the tile
            (assoc tile :sprite sprite)))))
    ;; Create the player's sprite
    (let [psprite (js/PIXI.Sprite. robot-texture)]
      (swap! state assoc :psprite psprite)
      (.addChild stage psprite)
      (set! (.-width psprite) 10)
      (set! (.-height psprite) 10))))

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
    (set! (.-x sprite) (* x 10))
    (set! (.-y sprite) (* y 10)))
  (.render renderer stage))
(render-tick!)
