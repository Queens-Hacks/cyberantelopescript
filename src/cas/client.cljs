(ns cas.client
  (:require [cas.world :as world]
            [cas.state :refer [state world-width world-height tile-dim]]
            [cas.render :refer [renderer stage tile-textures robot-texture]]))

(enable-console-print!)

(def keystate (atom {}))

(defn world-at [world x y] ((world x) (- world-height y)))

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

(defn setup! []
  (let [world (world/new-mountain-world world-width world-height 5 15)]
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
    ;; Create a player robot.
    (swap! state assoc :player (new-robot 3 3 player-script))
    ;; Create the player's sprite
    (let [psprite (js/PIXI.Sprite. robot-texture)]
      (swap! state assoc :psprite psprite)
      (.addChild stage psprite)
      (set! (.-width psprite) tile-dim)
      (set! (.-height psprite) tile-dim))))

(defn new-robot
  [x y script]
  {:pos {:x x :y y}
   :script script
   :pointer nil
   :response nil})

(defn is-key-down [key]
  (@keystate key))

(.addEventListener js/document "keydown"
                   (fn [e]
                     (. e preventDefault)
                     (swap! keystate assoc (.-keyCode e) true)))

(.addEventListener js/document "keyup"
                   (fn [e]
                     (swap! keystate assoc (.-keyCode e) false)))

(defn player-script
  [pointer response]
  {:command
   (cond (is-key-down 37) [:move :left]
         (is-key-down 38) [:move :up]
         (is-key-down 39) [:move :right]
         (is-key-down 40) [:move :down]
         :else nil)
   :pointer nil})

(defn up-right-script
  [pointer response]
  (cond (= pointer nil)
        {:command [:move :right] :pointer 0}
        (= pointer 0)
        {:command [:move :up] :pointer 1}
        (= pointer 1)
        {:command [:move :right] :pointer 0}
        :else nil))

(defn move-robot
  [world robot dir]
  (let [x-op (case dir :left dec :right inc identity)
        y-op (case dir :up dec :down inc identity)
        pos (:pos robot)]
    (cond (:passable (world-at world (x-op (:x pos)) (y-op (:y pos))))
          (update-in (update-in robot [:pos :y] y-op) [:pos :x] x-op)
          (and (or (= dir :left) (= dir :right))
               (:passable (world-at world (x-op (:x pos)) (dec (:y pos)))))
          (update-in (update-in robot [:pos :y] dec) [:pos :x] x-op)
          :else robot)))

(defn command-robot
  [world {:keys [:script :pointer :response] :as robot}]
  (let [resp (script pointer response)
        command (:command resp)
        pointer (:pointer resp)
        robot (assoc robot :pointer pointer)]
    (if command
      (let [kind (command 0) param (command 1)]
        (case kind
          :move (move-robot world robot param)))
      robot)))

(defn apply-gravity
  [world {:keys [pos] :as player}]
  (letfn [(air-below? []
            (:passable (world-at world (:x pos) (inc (:y pos)))))
          (air-beside? [dir]
            (let [op (case dir :left dec :right inc)]
              (:passable (world-at world (op (:x pos)) (:y pos)))))]
    (if (and (air-below?)
             (and (air-beside? :left) (air-beside? :right)))
      (update-in player [:pos :y] inc)
      player)))

(defn update-player! []
  (let [player (:player @state)
        world (:world @state)]
    (->> player
         (command-robot world)
         (apply-gravity world)
         (swap! state assoc :player))))

(defn update! []
  (update-player!))

;; Set up the event loop.
(defn tick! []
  (update!))

;; Set up the basic program structure.
(setup!)

(js/setInterval tick! 100)

(defn render-tick! []
  (js/requestAnimationFrame render-tick!)
  (let [sprite (:psprite @state)
        {:keys [x y]} (:pos (:player @state))]
    (set! (.-x sprite) (* x tile-dim))
    (set! (.-y sprite) (* y tile-dim)))
  (.render renderer stage))

(render-tick!)
