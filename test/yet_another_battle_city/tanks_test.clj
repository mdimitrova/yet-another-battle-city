(ns yet-another-battle-city.tanks-test
  (:use [clojure.test]
        [yet-another-battle-city.logic.tank])
  (:require [yet-another-battle-city.global-setup :as gs]))

(deftest tanks-test
  (let [tank (ref (define-tank 1 2 :left 3 :player))]
    (testing "is tank created"
      (is (and (= (@tank :position) [1 2])
               (= (@tank :direction) :up)
               (= (@tank :lives) 3)
               (= (@tank :type) :player))))
    (swap! tank (move tank))
    (testing "can tank move in it's direction"
      (is (= (@tank :position) [0, 2])))
    (swap! tank (turn tank :up))
    (testing "can tank turn"
      (is (= (@tank :direction) :up)))))
