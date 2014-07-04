(ns yet-another-battle-city.logic.tank
  (:require [yet-another-battle-city.global-setup :as gs]))

(defn define-tank
  "creates a new tank with the given position, direction and lives"
  [position direction lives tank-type]
  (let [tank {:position position :direction direction :lives lives :tank tank-type}]
    (reset! gs/enemy-tanks (conj @gs/enemy-tanks tank))
    (dissoc tank :position)))

(defn lose-live
  "decrements tank's lives"
  [tank]
  (assoc tank :lives (dec (tank :lives))))

(defn is-dead?
  "checks whether the tank is dead"
  [tank]
  (= (tank :lives) 0))
