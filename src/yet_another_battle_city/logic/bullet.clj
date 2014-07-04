(ns yet-another-battle-city.logic.bullet
  (:require [yet-another-battle-city.global-setup :as gs]))

(defn define-bullet
  "creates a new bullet with the given position and direction"
  [position direction]
  (let [bullet {:position position :direction direction}]
    (reset! gs/bullets (conj @gs/bullets bullet))
    (assoc (dissoc bullet :position) :bullet true)))
