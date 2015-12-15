(ns cas.state)

;; Basic properties
(def world-width 25)
(def world-height 20)
(def tile-dim 32)

(def state (atom {:world []
                  :player {:pos {:x 0 :y 0}
                           :mov-time [0 0]}}))
