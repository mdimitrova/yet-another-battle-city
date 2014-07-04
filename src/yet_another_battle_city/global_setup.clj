(ns yet-another-battle-city.global-setup)

(def field-width 13)
(def field-height 15)

(def directions {:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]})

(def enemy-tanks (atom []))
(def bullets (atom []))
