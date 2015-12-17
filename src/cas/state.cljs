(ns cas.state)

;; Basic properties
(def world-width 50)
(def world-height 40)
(def tile-dim 16)

(def state (atom {:world []
                  :player {}}))
