(ns yet-another-battle-city.logic.cli
  (:use [yet-another-battle-city.logic.level :as l]))

(defn get-symbol
  "return the corresponding symbol for the given item"
  [field [x y]]
  (let [item @(l/get-place field [x y])]
    (cond
      (= (item :obstacle) :brick) "="
      (= (item :obstacle) :steel) "+"
      (= (item :obstacle) :base) "b"
      (= (item :tank) :player) "@"
      (= (item :tank) :enemy) "t"
      (item :bullet) "."
      :else " ")))

(defn cli
  "print the given field in the console"
  [field]
  (let [result (atom "")
        height (count field)
        width (count (nth field 0))]
    (doseq [y (range height)]
      (do
        (reset! result (concat @result "|"))
        (doseq [x (range width)]
          (reset! result (concat @result (get-symbol field [x y]))))
        (reset! result (concat @result "|\n"))))
    (prn (apply str @result))))
