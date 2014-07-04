(ns yet-another-battle-city.logic.level
  (:use [yet-another-battle-city.logic.tank :as t]
        [yet-another-battle-city.logic.bullet :as b])
  (:require [yet-another-battle-city.global-setup :as gs]))

;; main level functions

(defn map-element
  "returns the element of the given map on the position"
  [level-map [x y]]
  (let [item (str (nth (nth level-map y) x))]
    (case item
      "#" {:obstacle :brick}
      "+" {:obstacle :steel}
      "@" (t/define-tank [x y] :up 3 :player)
      "t" (t/define-tank [x y] :up 1 :enemy)
      "b" {:obstacle :base}
      {})))

(defn define-field
  "defines a new field with the given dimentions and map"
  [width height level-map]
  (mapv (fn [y]
          (mapv (fn [x] (ref (map-element level-map [x y])))
                (range width)))
          (range height)))

(defn get-dimentions
  "return width and height of the given field"
  [field]
  (let [width (count (nth field 0))
        height (count field)]
    [width height]))

(defn get-place
  "returns the ref on the given position on the field"
  [field [x y]]
  (-> field (get y) (get x)))

;; level predicates

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

(defn is-player?
  "checks whether the position is the player"
  [field [x y]]
  (= (@(get-place field [x y]) :tank)
     :player))

(defn is-base?
  "checks whether the position is the base"
  [field [x y]]
  (= (@(get-place field [x y]) :obstacle)
     :base))

;; more level functions

(defn next-position
  "returns the object's next position in its current direction"
  [field [x y]]
  (let [object (get-place field [x y])]
    (mapv + [x y] (gs/directions (@object :direction)))))

(defn position-of
  "return the coorsinates if the given object on the field"
  [field, object]
  (let [[width height] (get-dimentions field)]
    (for [y (range height)]
      (for [x (range width)]
        (let [place (get-place field [x y])]
          (case object
            :player (if (is-player? field [x y])
                      [x y])
            :base (if (is-base? field [x y])
                    [x y])))))))

(defn abs [n] (max n (- n)))

(defn distance
  "returns the distance to the target in positions"
  [enemy-coords target-coords]
  (apply + (mapv abs (mapv - enemy-coords target-coords))))

(defn find-direction
  "returns the direction in which the enemy tank should move"
  [field [enemy-x enemy-y] [target-x target-y]]
  (cond
   (< enemy-x target-x) :right
   (> enemy-x target-x) :left
   (< enemy-y target-y) :down
   :else :up))

(defn closest-target-direction
  "return the direction in which the enemy tank should move to reach the closest target"
  [field [enemy-x enemy-y]]
  (let [player-position (position-of field :player)
        player-distance (distance [enemy-x enemy-y] player-position)
        base-position (position-of field :base)
        base-distance (distance [enemy-x enemy-y] base-position)]
    (if (< base-distance player-distance)
      (find-direction field [enemy-x enemy-y] base-position)
      (find-direction field [enemy-x enemy-y] player-position))))

;; object functions

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

;; bullet functions

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

(defn update-bullets
  "moves all the bullets on the field by one step"
  [field]
  (dosync
   (let [[width height] (get-dimentions field)]
     (for [y (range height)]
       (for [x (range width)]
         (if (is-bullet? field [x y])
           (move-bullet field [x y])))))))

;; enemy tanks functions

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
       (ref-set new-place (b/define-bullet new-location direction))))))

(defn move-tank
  "moves the tank on the given position on the field in its direction"
  [field [x y]]
  (dosync
   (let [old-place (get-place field [x y])
         tank-info @old-place
         new-location (next-position field [x y])
         new-place (get-place field new-location)]
     (if-not (or
              (is-bullet? field new-location)
              (is-out? field new-location)
              (is-blocking? field new-location))
       (do
         (ref-set new-place tank-info)
         (ref-set old-place {}))))))

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

(defn turn-enemy-tanks
  "turns all the enemy tanks towards their closest target"
  [field]
  (dosync
   (let [[width height] (get-dimentions field)]
     (for [y (range height)]
       (for [x (range width)]
         (if (is-enemy? field [x y])
           (turn-to field [x y] (closest-target-direction field [x y]))))))))

(defn move-enemy-tanks
  "moves all the enemy tanks by one step"
  [field]
  (dosync
   (let [[width height] (get-dimentions field)]
     (for [y (range height)]
       (for [x (range width)]
         (if (is-enemy? field [x y])
             (move-tank field [x y])))))))

(defn shoot-enemy-tanks
  "makes all the enemy tanks shoot"
  [field]
  (dosync
   (let [[width height] (get-dimentions field)]
     (for [y (range height)]
       (for [x (range width)]
         (if (and
              (is-enemy? field [x y])
              (not (is-bullet? field (next-position field [x y]))))
           (shoot field [x y])))))))
