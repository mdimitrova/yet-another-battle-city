(ns yet-another-battle-city.logic.tank
  (:require [yet-another-battle-city.global-setup :as gs]))

(defn define-tank
  "creates a new tank on the position with the gives direction and lives"
  [x y direction lives tank-type]
  {:position [x y] :direction direction :lives lives :type tank-type})

(defn turn
  "turn the tank into the given direction"
  [tank direction]
  (assoc tank :direction direction))

(defn next-position
  "returns the object's next position in its current direction"
  [tank]
  (mapv + (tank :position) (gs/directions (tank :direction))))

(defn move
  "move the tank on step into the given direction"
  [tank]
  (assoc tank :position (next-position tank)))

(defn lose-live
  "decrements tanks' lives"
  [tank]
  (assoc tank :lives (dec (tank :lives))))

(defn is-dead?
  "checks whether the tank is dead"
  [tank]
  (= (tank :lives) 0))
