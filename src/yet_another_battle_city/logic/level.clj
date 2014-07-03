(ns yet-another-battle-city.logic.level
  (:use [yet-another-battle-city.logic.tank :as t])
  (:require [yet-another-battle-city.global-setup :as gs]))

(defn map-element
  "returns the element of the given map on the position"
  [level-map [x y]]
  (let [item (str (nth (nth level-map y) x))]
    (case item
      "=" {:obstacle :brick}
      "+" {:obstacle :steel}
      "@" (t/define-tank :up 3 :player)
      "t" (t/define-tank :up 1 :enemy)
      "b" (:obstacle :base)
      {})))

(defn define-field
  "defines a new field with the given dimentions and map"
  [width height level-map]
  (mapv (fn [y]
          (mapv (fn [x] (ref (map-element level-map [x y])))
                (range width)))
          (range height)))

(defn get-place
  "returns the ref on the given position on the field"
  [field [x y]]
  (-> field (get y) (get x)))

(defn next-position
  "returns the object's next position in its current direction"
  [field [x y]]
  (let [object (get-place field [x y])]
    (mapv + [x y] (gs/directions (@object :direction)))))

(defn get-dimentions
  "return width and height of the given field"
  [field]
  (let [width (count (nth field 0))
        height (count field)]
    [width height]))

(defn is-out?
  "checks whether the position is outside the world"
  [field [x y]]
  (nil? (get-place field [x y])))

(defn is-blocking?
  "checks whether the position is an obstacle or a tank"
  [field [x y]]
  (or (contains? @(get-place field [x y]) :obstacle)
      (contains? @(get-place field [x y]) :tank)))

(defn is-bullet?
  "checks whether the position is a bullet"
  [field [x y]]
  (contains? @(get-place field [x y]) :bullet))

(defn is-enemy?
  "checks whether the position is an enemy"
  [field [x y]]
  (= (@(get-place field [x y]) :tank)
     :enemy))

(defn turn-to
  "turn the object on the given position on the field in the given direction"
  [field [x y] direction]
  (dosync
    (let [object (get-place field [x y])]
     (ref-set object (assoc @object :direction direction)))))

(defn destroy
  "destroys if the given position is bricks"
  [field [x y]]
  (dosync
   (let [place (get-place field [x y])]
     (if (= (@place :obstacle) :brick)
       (ref-set place {})))))

(defn move-tank
  "moves the tank on the given position on the field in its direction"
  [field [x y]]
  (dosync
   (let [old-place (get-place field [x y])
         tank-info @old-place
         new-location (next-position field [x y])
         new-place (get-place field new-location)]
     (if-not (is-blocking? field new-location)
       (do
         (ref-set new-place tank-info)
         (ref-set old-place {}))))))

(defn move-bullet
  "moves the bullet or destroys"
  [field [x y]]
  (dosync
   (let [old-place (get-place field [x y])
         bullet-info @old-place
         new-location (next-position field [x y])
         new-place (get-place field new-location)]
     (if (is-blocking? field new-location)
       (destroy field new-location)
       (ref-set new-place bullet-info))
     (ref-set old-place {}))))

(defn shoot
  "make shoot the object on the given position shoot"
  [field [x y]]
  (dosync
   (let [old-place (get-place field [x y])
         direction (@old-place :direction)
         new-location (next-position field [x y])
         new-place (get-place field new-location)]
     (if (is-blocking? field new-location)
       (destroy field new-location)
       (ref-set new-place {:bullet true :direction direction})))))

(defn update-bullets
  "moves all the bullets on the field by one step"
  [field]
  (dosync
   (let [[width height] (get-dimentions field)]
     (for [y (range height)]
       (for [x (range width)]
         (if (is-bullet? field [x y])
           (move-bullet field [x y])))))))

(defn remove-dead-tanks
  "removes tanks with zero lives"
  [field]
  (dosync
   (let [[width height] (get-dimentions field)]
     (for [y (range height)]
       (for [x (range width)]
         (let [place (get-place field [x y])]
           (if (and (is-enemy? field [x y])
                    (t/is-dead? place))
             (ref-set place {}))))))))

(defn move-shoot-enemy-tanks ; todo move towards closest target (player or base)
  "moves all the enemy tanks by one step and makes the shoot"
  [field]
  (dosync
   (let [[width height] (get-dimentions field)]
     (for [y (range height)]
       (for [x (range width)]
         (if (is-enemy? field [x y])
           (do
             (move-tank field [x y])
             (shoot field [x y]))))))))

(defn update-field
  "update field's state"
  [field]
  (-> field
      ;remove-dead-tanks
      ;move-shoot-enemy-tanks
      update-bullets))
