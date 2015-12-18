(ns cas.client
  (:require [cas.world :as world]
            [cas.state :refer [state world-width world-height tile-dim]]
            [cas.render :refer [renderer stage tile-textures robot-texture]]
            [cas.util :refer [tile-at each-tile map-tiles]]))

(enable-console-print!)

(def keystate (atom {}))

(def world-at tile-at)

(defn set-world-at!
  [x y kind]
  (let [tile (world-at (:world @state) x y)
        sprite (:sprite tile)
        tex (kind tile-textures)]
    (set! (.-texture sprite) tex)
    (swap! state update-in
           [:world x y :kind]
           (fn [] kind))
    (swap! state update-in
           [:world x y :passable]
           (fn [] (world/passability kind)))))

(defn new-robot
  [x y script]
  {:pos {:x x :y y}
   :script script
   :pointer nil
   :response nil})

(defn setup! []
  (let [world (world/new-chunk world-width world-height)]
    (swap! state assoc :world
      (map-tiles world
        (fn [x y tile]
          (let [tex ((:kind tile) tile-textures)
                sprite (js/PIXI.Sprite. tex)]
            ;; Add the sprite to the stage
            (.addChild stage sprite)
            (set! (.-x sprite) (* x tile-dim))
            (set! (.-y sprite) (* y tile-dim))
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
         (is-key-down 87) [:dig :up]
         (is-key-down 83) [:dig :down]
         (is-key-down 65) [:dig :left]
         (is-key-down 68) [:dig :right]
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
               (:passable (world-at world (x-op (:x pos)) (dec (:y pos))))
               (:passable (world-at world (:x pos) (dec (:y pos)))))
          (update-in (update-in robot [:pos :y] dec) [:pos :x] x-op)
          :else robot)))

(defn dig-robot!
  [world robot dir]
  (let [pos (:pos robot)]
    (case dir
      :down (set-world-at! (pos :x) (inc (pos :y)) :air)
      :up (set-world-at! (pos :x) (dec (pos :y)) :air)
      :left (set-world-at! (dec (pos :x)) (pos :y) :air)
      :right (set-world-at! (inc (pos :x)) (pos :y) :air))
    robot))

(defn command-robot
  [world {:keys [:script :pointer :response] :as robot}]
  (let [resp (script pointer response)
        command (:command resp)
        pointer (:pointer resp)
        robot (assoc robot :pointer pointer)]
    (if command
      (let [kind (command 0) param (command 1)]
        (case kind
          :move (move-robot world robot param)
          :dig (dig-robot! world robot param)))
      robot)))

(defn apply-gravity
  [world {:keys [pos] :as player}]
  (if (.-godMode js/window)
    player
    (letfn [(air-below? []
              (:passable (world-at world (:x pos) (inc (:y pos)))))
            (air-beside? [dir]
              (let [op (case dir :left dec :right inc)]
                (:passable (world-at world (op (:x pos)) (:y pos)))))]
      (if (and (air-below?)
            (and (air-beside? :left) (air-beside? :right)))
        (update-in player [:pos :y] inc)
        player))))

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

(set-world-at! 3 4 :stone)
(set-world-at! 3 4 :air)

(js/setInterval tick! 100)

(defn render-tick! []
  (js/requestAnimationFrame render-tick!)
  (let [sprite (:psprite @state)
        {:keys [x y]} (:pos (:player @state))]
    (set! (.-x sprite) (* x tile-dim))
    (set! (.-y sprite) (* y tile-dim))
    (let [half-player (/ tile-dim 2)
          pixel-px (+ (* x tile-dim) half-player)
          pixel-py (+ (* y tile-dim) half-player)
          half-viewx (/ 800 2)
          half-viewy (/ 600 2)
          offset-x (- half-viewx pixel-px)
          offset-y (- half-viewy pixel-py)]
      (set! (.-x stage) offset-x)
      (set! (.-y stage) offset-y)))
  (.render renderer stage))

(render-tick!)
