(ns yet-another-battle-city.logic.level
  (:require [yet-another-battle-city.global-setup :as gs]))

(defn map-element
  "returns the element of the given map on position (x, y)"
  [level-map x y]
  (let [item (str (nth (nth level-map y) x))]
    (case item
      "=" {:obstacle :brick}
      "+" {:obstacle :steel}
      {})))

(defn define-field
  "defines a new field with the given dimentions and map"
  [width height level-map]
  (mapv (fn [y]
          (mapv (fn [x] (ref (map-element level-map x y)))
                (range width)))
          (range height)))

(defn get-position
  "returns the object on the given position on the field"
  [field x y]
  (get (get field y) x))

(defn is-out?
  "checks whether the position is outside the world"
  [field x y]
  (nil? (get-position field x y)))

(defn is-obstacle?
  "checks whether the position is an obstacle"
  [field x y]
  (contains? @(get-position field x y) :obstacle))
