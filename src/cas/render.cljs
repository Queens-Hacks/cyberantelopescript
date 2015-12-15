(ns cas.render)

(enable-console-print!)

;; Create the pixi stage
(def renderer
  (.autoDetectRenderer js/PIXI 800 600 #js {:backgroundColor 0x1099bb}))
(.appendChild (.. js/document -body) (.-view renderer))

(def stage (js/PIXI.Container.))

(defn- mk-texture [path]
  (let [tex (js/PIXI.Texture.fromImage
              path false
              js/PIXI.SCALE_MODES.NEAREST)]
    ; (set! (.-scaleMode tex) js/PIXI.SCALE_MODES.NEAREST)
    (println (.-scaleMode tex))
    tex))
(def tile-textures {:air (mk-texture "tiles/air.png")
                    :dirt (mk-texture "tiles/dirt.png")
                    :stone (mk-texture "tiles/stone.png")})
(def robot-texture (mk-texture "tiles/robot.png"))



