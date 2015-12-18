(ns cas.state)

;; Basic properties
(def world-width 200)
(def world-height 160)
(def tile-dim 8)

(def state (atom {:world []
                  :player {}}))
