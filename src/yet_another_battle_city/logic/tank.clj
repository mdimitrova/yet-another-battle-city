(ns yet-another-battle-city.logic.tank
  (:require [yet-another-battle-city.global-setup :as gs]))

(defn define-tank
  "creates a new tank with the gives direction and lives"
  [direction lives tank-type]
  {:direction direction :lives lives :tank tank-type})

(defn lose-live
  "decrements tank's lives"
  [tank]
  (assoc tank :lives (dec (tank :lives))))

(defn is-dead?
  "checks whether the tank is dead"
  [tank]
  (= (tank :lives) 0))
