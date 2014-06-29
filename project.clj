(defproject yet-another-battle-city "0.1.0-SNAPSHOT"
  :description "Remake of the classic video game Battle City for the Clojure course at FMI"
  :url "https://github.com/mdimitrova/yet-another-battle-city"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot yet-another-battle-city.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
